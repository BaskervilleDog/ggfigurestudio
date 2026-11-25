(() => {
  window.ggfigurestudioPositionMenu = function () {
    if (!this.isOpen) return;
    const anchor = this.$control[0].getBoundingClientRect();
    const menu = this.$dropdown[0];
    const list = this.$dropdown_content[0];
    const viewport = document.documentElement;
    const inset = 8;
    const width = Math.min(anchor.width, viewport.clientWidth - 2 * inset);
    const left = Math.min(Math.max(inset, anchor.left), viewport.clientWidth - inset - width);
    Object.assign(menu.style, {
      position: "fixed", width: `${width}px`, minWidth: "0", left: `${left}px`,
      top: `${anchor.bottom}px`, bottom: "auto"
    });

    const border = menu.offsetHeight - list.offsetHeight;
    const below = Math.max(0, viewport.clientHeight - anchor.bottom - inset);
    const above = Math.max(0, anchor.top - inset);
    const desired = Math.min(list.scrollHeight, 288) + border;
    const opensAbove = below < desired && above > below;
    const available = opensAbove ? above : below;
    list.style.maxHeight = `${Math.max(0, Math.min(288, available - border))}px`;
    menu.style.top = `${opensAbove ? anchor.top - menu.offsetHeight : anchor.bottom}px`;
  };

  const reposition = event => {
    for (const input of document.querySelectorAll(".editor-shell select.selectized")) {
      const control = input.selectize;
      if (!control?.isOpen) continue;
      if (event.type === "scroll") {
        if (!control.$dropdown[0].contains(event.target)) control.close();
      } else {
        control.positionDropdown();
      }
    }
  };
  document.addEventListener("scroll", reposition, true);
  window.addEventListener("resize", reposition);
  document.addEventListener("DOMContentLoaded", () => {
    // Shiny can replace inspector inputs while their menus live under body.
    window.jQuery(document).on("shiny:unbound", ".editor-shell select.selectized", function () {
      if (this.selectize?.settings.dropdownClass === "editor-select-menu") this.selectize.destroy();
    });
  });
})();
