(() => {
  const pending = new Map();
  const returnFocus = new Map();
  const draftFocus = new Map();
  const dialogFocus = new Map();
  const scrollSelectors = [
    ".llw-metadata-result-list", ".llw-metadata-property-list",
    ".llw-metadata-item-properties", ".llw-metadata-focus-contents",
    ".llw-metadata-group-values"
  ];
  const ownedDialogs = [
    [".llw-metadata-path-overflow", ".llw-metadata-navigation-dialog"],
    [".llw-overlay-edit", ".llw-overlay-draft"]
  ];

  // The dependent native select has a browser-owned generation. Context
  // equality alone is insufficient when a user changes A -> B -> A.
  const sectionStates = new WeakMap();
  const sectionState = grid => {
    if (!sectionStates.has(grid)) {
      sectionStates.set(grid, { epoch: 0, generation: 0, pending: true,
        resource: "all", parents: null });
    }
    return sectionStates.get(grid);
  };
  const sectionElements = grid => ({
    source: document.getElementById(grid.dataset.sectionSource),
    layer: document.getElementById(grid.dataset.sectionLayer),
    select: document.getElementById(grid.dataset.sectionInput),
    slot: grid.querySelector(".llw-metadata-section-slot"),
    status: grid.querySelector(".llw-metadata-section-status"),
    feedback: grid.querySelector(".llw-metadata-section-feedback"),
    retry: grid.querySelector(".llw-metadata-section-retry")
  });
  const sectionPending = (grid, text = "Updating sections...") => {
    const el = sectionElements(grid);
    el.select.disabled = true;
    el.slot.hidden = false;
    el.select.setAttribute("aria-busy", "true");
    el.feedback.hidden = false;
    el.status.textContent = text;
  };
  const requestSections = grid => {
    const el = sectionElements(grid);
    if (!el.select || !el.source || !el.layer) return;
    const state = sectionState(grid);
    if (!state.pending) state.resource = el.select.value;
    state.pending = true;
    state.generation += 1;
    state.parents = [el.source.value, el.layer.value];
    sectionPending(grid);
    Shiny.setInputValue(grid.dataset.sectionRequest, {
      epoch: state.epoch, generation: state.generation,
      namespace: el.source.value, layer: el.layer.value, resource: state.resource
    }, { priority: "event" });
  };
  let sectionsRegistered = false;
  const registerSections = () => {
    if (sectionsRegistered) return;
    sectionsRegistered = true;
    Shiny.addCustomMessageHandler("llw-metadata-section", message => {
      if (!message || typeof message.target !== "string" ||
          !Number.isSafeInteger(message.epoch) || message.epoch < 1) return;
      const grid = document.getElementById(message.target);
      if (!grid?.matches(".llw-metadata-filter-grid[data-section-request]")) return;
      const state = sectionState(grid);
      const el = sectionElements(grid);
      if (message.kind === "context") {
        if (message.epoch <= state.epoch) return;
        state.epoch = message.epoch;
        if (message.reset || typeof message.selected === "string") {
          state.resource = message.reset ? "all" : message.selected;
          state.pending = true;
        }
        requestSections(grid);
        return;
      }
      if (!state.pending || message.epoch !== state.epoch ||
          message.generation !== state.generation) return;
      if (message.kind === "error") {
        sectionPending(grid, "Sections could not be updated. Retry sections.");
        return;
      }
      if (message.kind !== "choices" ||
          message.namespace !== el.source.value || message.layer !== el.layer.value ||
          !Array.isArray(message.choices) || message.choices.length === 0 ||
          typeof message.selected !== "string" || typeof message.visible !== "boolean" ||
          !message.choices.every(x => x && typeof x.value === "string" &&
            typeof x.label === "string") ||
          !message.choices.some(x => x.value === message.selected)) return;
      const retryFocused = document.activeElement === el.retry;
      const options = document.createDocumentFragment();
      for (const choice of message.choices) {
        const option = document.createElement("option");
        option.value = choice.value;
        option.textContent = choice.label;
        options.appendChild(option);
      }
      el.select.replaceChildren(options);
      el.select.value = message.selected;
      state.resource = message.selected;
      state.pending = false;
      el.select.setAttribute("aria-busy", "false");
      el.feedback.hidden = true;
      el.status.textContent = "";
      el.slot.hidden = !message.visible;
      el.select.disabled = !message.visible;
      // Use the existing Shiny native binding. A reply is consumed before
      // notifying it, so a duplicate cannot overwrite a newer ready choice.
      el.select.dispatchEvent(new Event("change", { bubbles: true }));
      if (retryFocused) {
        const target = message.visible ? el.select :
          [el.source, el.layer, ...grid.querySelectorAll("select")]
            .find(node => !node.disabled && node.getClientRects().length > 0);
        target?.focus({ preventScroll: true });
      }
    });
    window.jQuery(document).on("change.llwMetadataSections",
      ".llw-metadata-filter-grid select", function () {
        const grid = this.closest(".llw-metadata-filter-grid[data-section-request]");
        if (!grid) return;
        const state = sectionState(grid);
        const el = sectionElements(grid);
        if (this === el.select) {
          if (!state.pending) state.resource = this.value;
          return;
        }
        if (this !== el.source && this !== el.layer) return;
        const parents = [el.source.value, el.layer.value];
        if (state.parents?.every((value, i) => value === parents[i])) return;
        requestSections(grid);
      });
    window.jQuery(document).on("click.llwMetadataSections",
      ".llw-metadata-section-retry", function () {
        requestSections(this.closest(".llw-metadata-filter-grid"));
      });
    window.jQuery(document).on("shiny:connected.llwMetadataSections", () => {
      document.querySelectorAll(".llw-metadata-filter-grid[data-section-request]")
        .forEach(grid => {
          sectionState(grid).epoch = 0;
          sectionElements(grid).retry.disabled = false;
          requestSections(grid);
        });
    });
    window.jQuery(document).on("shiny:disconnected.llwMetadataSections", () => {
      document.querySelectorAll(".llw-metadata-filter-grid[data-section-request]")
        .forEach(grid => {
          sectionState(grid).pending = true;
          sectionPending(grid, "Connection lost. Sections will update after reconnecting.");
          sectionElements(grid).retry.disabled = true;
        });
    });
  };

  // Native Shiny owns the controls. This handler only positions and focuses
  // the resolved destination after dynamic output and tab changes settle.
  const register = () => {
    registerSections();
    // This message only focuses controls in this owned dialog. The server
    // owns the confirmation state and every Apply/Discard transaction.
    Shiny.addCustomMessageHandler("llw-overlay-dialog-focus", (message) => {
      if (!message || typeof message.target !== "string" ||
          typeof message.keep !== "string" || typeof message.review !== "string" ||
          !["confirm", "resume", "review"].includes(message.mode)) return;
      dialogFocus.set(message.target, message);
      const started = performance.now();
      const focus = () => {
        if (dialogFocus.get(message.target) !== message) return;
        const draft = document.getElementById(message.target);
        if (!draft?.matches(".llw-overlay-draft")) return;
        const modal = draft.closest(".modal.show");
        const target = message.mode === "confirm" ?
          document.getElementById(message.keep) : message.mode === "review" ?
          document.getElementById(message.review) : draftFocus.get(message.target);
        // A task button temporarily disables itself and can release focus to
        // body. Recover only that loss, never a user's newer dialog choice.
        const active = document.activeElement;
        if (message.mode === "review" && modal &&
            (modal.querySelector(".llw-overlay-close-confirmation") ||
             (modal.contains(active) && active !== modal && active !== target))) return;
        if (modal && target?.isConnected && modal.contains(target) &&
            !target.disabled && target.getClientRects().length > 0 &&
            (message.mode !== "review" ||
             !document.documentElement.classList.contains("shiny-busy"))) {
          target.focus();
        } else if (performance.now() - started < 1000) {
          requestAnimationFrame(focus);
        }
      };
      requestAnimationFrame(focus);
    });
    Shiny.addCustomMessageHandler("llw-metadata-focus", (message) => {
      if (!message || typeof message.target !== "string" ||
          typeof message.group_id !== "string") return;
      const previous = pending.get(message.target);
      if (previous) cancelAnimationFrame(previous.frame);
      const request = { frame: null, started: performance.now(), stable: 0 };
      pending.set(message.target, request);
      const position = () => {
        const target = document.getElementById(message.target);
        const content = target?.querySelector("[data-selected-group]");
        const ready = target && target.getClientRects().length > 0 &&
          content?.dataset.selectedGroup === message.group_id &&
          !document.querySelector(".modal.show") &&
          !document.documentElement.classList.contains("shiny-busy");
        if (ready) {
          const top = target.getBoundingClientRect().top;
          request.stable = top === request.top ? request.stable + 1 : 0;
          request.top = top;
          if (request.stable >= 2) {
            target.focus({ preventScroll: true });
            target.scrollIntoView({ block: "start", behavior: "instant" });
            pending.delete(message.target);
            return;
          }
        }
        if (performance.now() - request.started < 3000) {
          request.frame = requestAnimationFrame(position);
        } else {
          pending.delete(message.target);
        }
      };
      request.frame = requestAnimationFrame(position);
    });
  };

  if (window.Shiny) register();
  else document.addEventListener("shiny:connected", register, { once: true });

  // Shiny opens modals from R, so Bootstrap has no trigger element to return
  // to. Remember the page/list viewport for dismissal and overlay commits.
  // Full-path choices keep their independent destination-scrolling behavior.
  document.addEventListener("click", (event) => {
    for (const [opener, dialog] of ownedDialogs) {
      const button = event.target.closest?.(opener);
      if (!button || button.disabled) continue;
      const root = button.closest(".llw-metadata-explorer");
      const scrolls = root ?
        scrollSelectors.flatMap(selector => [...root.querySelectorAll(selector)].map(node => {
          const owner = node.closest(".shiny-html-output[id]");
          return { node, owner: owner?.id, selector,
            index: owner ? [...owner.querySelectorAll(selector)].indexOf(node) : 0,
            top: node.scrollTop, left: node.scrollLeft };
        })) : [];
      returnFocus.set(dialog, { button, scrolls, x: window.scrollX, y: window.scrollY });
    }
  });
  // Bootstrap may finish hiding before a document click listener runs.
  // Mark native dismissal from its initiating gesture, without replacing it.
  const markPathDismissal = event => {
    const modal = event.target.closest?.(".modal.show");
    if (modal?.querySelector(".llw-metadata-navigation-dialog") &&
        (event.target === modal ||
         event.target.closest?.('[data-bs-dismiss="modal"], [data-dismiss="modal"]'))) {
      const saved = returnFocus.get(".llw-metadata-navigation-dialog");
      if (saved) saved.dismissed = true;
    }
  };
  document.addEventListener("pointerdown", markPathDismissal, true);
  document.addEventListener("change", event => {
    if (event.target.matches?.('input[type="radio"]') &&
        event.target.closest(".llw-metadata-navigation-dialog")) {
      const saved = returnFocus.get(".llw-metadata-navigation-dialog");
      if (saved) saved.dismissed = false;
    }
  }, true);
  // Native easyClose cannot protect unblurred inputs or notify the server to
  // clear its draft. Escape sends a dismissal request, never an Apply event.
  // An open selectize dropdown retains its native first-Escape behavior.
  document.addEventListener("keydown", (event) => {
    if (!event.isComposing && !event.defaultPrevented &&
        (event.key === "Enter" || event.key === " ")) markPathDismissal(event);
    if (event.key !== "Escape" || event.isComposing || event.defaultPrevented) return;
    // Busy controls may release focus outside the dialog before resetting.
    // Only the top visible modal may own Escape; other popup types are ignored.
    const modal = event.target.closest?.(".modal.show") ||
      [...document.querySelectorAll(".modal.show")]
        .filter(node => node.getClientRects().length > 0).at(-1);
    if (modal?.querySelector(".llw-metadata-navigation-dialog")) {
      const saved = returnFocus.get(".llw-metadata-navigation-dialog");
      if (saved) saved.dismissed = true;
      return;
    }
    const draft = modal?.querySelector(".llw-overlay-draft[data-escape-input]");
    if (!draft || !window.Shiny) return;
    const openDropdown = [...modal.querySelectorAll(".selectize-dropdown")]
      .some(dropdown => dropdown.getClientRects().length > 0);
    if (openDropdown) return;
    event.preventDefault();
    event.stopPropagation();
    if (!modal.querySelector(".llw-overlay-close-confirmation")) {
      const active = event.target;
      const fallback = draft.querySelector(".llw-overlay-value-input input:not([disabled])");
      draftFocus.set(draft.id,
        modal.contains(active) && active !== modal ? active : fallback || modal);
    }
    const values = [...draft.querySelectorAll(".llw-overlay-value-input input")];
    const dirty = values.some(input => input.value.length > 0) ||
      !!draft.querySelector('input[type="radio"][value="remove"]:checked') ||
      !!draft.querySelector('input[type="checkbox"]:checked');
    Shiny.setInputValue(draft.dataset.escapeInput, { dirty }, { priority: "event" });
  }, true);
  if (window.jQuery) {
    window.jQuery(document).on("hidden.bs.modal.llwMetadata", (event) => {
      for (const [, dialog] of ownedDialogs) {
        if (!event.target.querySelector(dialog)) continue;
        const saved = returnFocus.get(dialog);
        returnFocus.delete(dialog);
        const button = saved?.button;
        const retainViewport = dialog === ".llw-overlay-draft" || saved?.dismissed;
        const draft = event.target.querySelector(".llw-overlay-draft");
        if (draft) {
          draftFocus.delete(draft.id);
          dialogFocus.delete(draft.id);
        }
        const restore = () => {
          if (!saved || !retainViewport) return;
          for (const scroll of saved.scrolls) {
            const node = scroll.node.isConnected ? scroll.node :
              document.getElementById(scroll.owner)?.querySelectorAll(scroll.selector)[scroll.index];
            if (node) { node.scrollTop = scroll.top; node.scrollLeft = scroll.left; }
          }
          window.scrollTo({ left: saved.x, top: saved.y, behavior: "instant" });
        };
        restore();
        if (button?.isConnected && !button.disabled &&
            button.getClientRects().length > 0) {
          button.focus({ preventScroll: true });
        }
        if (saved && retainViewport) {
          // Output replacement and native keyboard dismissal can adjust scroll
          // after Bootstrap's hidden event. Retain it through that bounded
          // settling period, yielding to the user's next navigation gesture.
          const started = performance.now();
          let changed = started;
          let cancelled = false;
          const root = button?.closest(".llw-metadata-explorer");
          const observer = new MutationObserver(() => { changed = performance.now(); });
          if (root) observer.observe(root, { childList: true, subtree: true });
          const gestures = ["wheel", "touchstart", "pointerdown", "keydown"];
          // A synchronous native Close can still be dispatching its keydown.
          // That initiating gesture must not cancel its own restoration.
          const cancel = event => {
            if (event.timeStamp > started) cancelled = true;
          };
          gestures.forEach(type => document.addEventListener(type, cancel, { passive: true }));
          const settle = () => {
            const now = performance.now();
            if (!cancelled) restore();
            const ready = now - started >= 150 && now - changed >= 100 &&
              !document.documentElement.classList.contains("shiny-busy");
            if (!cancelled && !ready && now - started < 1000) {
              requestAnimationFrame(settle);
            } else {
              observer.disconnect();
              gestures.forEach(type => document.removeEventListener(type, cancel));
            }
          };
          requestAnimationFrame(settle);
        }
      }
    });
  }
})();
