// Presentation only: position and focus an explicitly selected Prepare view.
// Recipe state and tab selection remain owned by Shiny.
(() => {
  const pending = new Map();
  const gestures = ["pointerdown", "keydown", "wheel", "touchstart"];
  const register = () => {
    Shiny.addCustomMessageHandler("llw-prepare-flow-navigate", message => {
      if (!message || typeof message.root !== "string" ||
          typeof message.target !== "string") return;
      pending.get(message.root)?.();
      let frame;
      let stable = 0;
      let previousTop;
      const started = performance.now();
      const stop = () => {
        cancelAnimationFrame(frame);
        gestures.forEach(type => document.removeEventListener(type, stop));
        if (pending.get(message.root) === stop) pending.delete(message.root);
      };
      pending.set(message.root, stop);
      // A later user gesture takes precedence over a delayed navigation reply.
      gestures.forEach(type => document.addEventListener(type, stop, { passive: true }));
      const position = () => {
        const root = document.getElementById(message.root);
        const target = document.getElementById(message.target);
        const main = target?.closest(".llw-flow-main");
        const pane = target?.closest(".tab-pane");
        const ready = root?.matches(".llw-prepare-flow") &&
          target?.matches(".llw-flow-destination") && root.contains(target) &&
          main && pane?.classList.contains("active") &&
          target.getClientRects().length > 0 && !document.querySelector(".modal.show");
        if (ready) {
          const top = main.getBoundingClientRect().top;
          stable = top === previousTop ? stable + 1 : 0;
          previousTop = top;
          if (stable >= 2) {
            // Include status above the heading; do not hide validation/busy feedback.
            const feedback = root.querySelector(".llw-flow-feedback");
            (feedback || main).scrollIntoView({ block: "start", behavior: "instant" });
            target.focus({ preventScroll: true });
            stop();
            return;
          }
        }
        if (performance.now() - started < 1000) {
          frame = requestAnimationFrame(position);
        } else stop();
      };
      frame = requestAnimationFrame(position);
    });
  };
  if (window.Shiny) register();
  else window.jQuery(document).one("shiny:connected.llwPrepareFlow", register);
})();
