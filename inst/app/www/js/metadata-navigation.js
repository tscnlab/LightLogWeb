(() => {
  const pending = new Map();
  let returnFocus = null;

  // Native Shiny owns the controls. This handler only positions and focuses
  // the resolved destination after dynamic output and tab changes settle.
  const register = () => {
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
  // to. Keep keyboard focus in this module when its navigation dialog closes.
  document.addEventListener("click", (event) => {
    const button = event.target.closest?.(
      ".llw-metadata-path-overflow"
    );
    if (button) returnFocus = button;
  });
  if (window.jQuery) {
    window.jQuery(document).on("hidden.bs.modal.llwMetadata", (event) => {
      if (!event.target.querySelector(".llw-metadata-navigation-dialog")) return;
      if (returnFocus?.isConnected) returnFocus.focus({ preventScroll: true });
      returnFocus = null;
    });
  }
})();
