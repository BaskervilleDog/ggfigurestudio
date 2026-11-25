(() => {
  let drag = null;
  let canvasFocused = false;
  let selected = "";
  let selectedMark = "";
  let selectedMove = "";
  let selectedMoveAxis = "";
  let lastCanvas = null;
  let lastTileScope = "";
  const tileScope = () => document.querySelector("#tile_scope input:checked")?.value || "";
  const buttons = () => Array.from(document.querySelectorAll(".figure-canvas button[data-key]"));
  const inspect = key => Shiny.setInputValue("inspect_element", key, {priority:"event"});
  function highlight() {
    const canvas = document.querySelector(".figure-canvas");
    if (!canvas) return;
    let outlinedLabel = false;
    for (const item of buttons()) {
      const active = item.dataset.key === selected ||
        (selectedMark !== "" && item.dataset.inspectMark === selectedMark);
      item.style.outline = active ? "2px solid #396abe" : "";
      item.setAttribute("aria-pressed", String(active));
      if (active && item.dataset.inspectMark) outlinedLabel = true;
    }
    const overlay = canvas.querySelector(".figure-mark-selection");
    overlay.replaceChildren();
    if (!selectedMark || outlinedLabel) return;
    const areaLayer = /^(tile|area|bar)\//.test(selectedMark) && tileScope() === "layer" ?
      selectedMark.split("/").slice(0, 2).join("/") + "/" : null;
    for (const hit of canvas.querySelectorAll(".figure-mark-hit")) {
      if (areaLayer ? !hit.dataset.inspect.startsWith(areaLayer) : hit.dataset.inspect !== selectedMark)
        continue;
      const outline = document.createElementNS("http://www.w3.org/2000/svg", "path");
      outline.setAttribute("d", hit.dataset.outline);
      outline.setAttribute("fill", "none");
      const clip = hit.getAttribute("clip-path");
      if (clip) outline.setAttribute("clip-path", clip);
      outline.setAttribute("stroke", "#396abe");
      outline.setAttribute("stroke-width", "1.5");
      outline.setAttribute("vector-effect", "non-scaling-stroke");
      overlay.append(outline);
    }
  }
  const select = button => {
    selected = button.dataset.key;
    selectedMark = button.dataset.inspectMark || "";
    selectedMove = "";
    selectedMoveAxis = "";
    highlight();
  };
  const selectMovableMark = hit => {
    selected = "";
    selectedMark = hit.dataset.inspect;
    selectedMove = hit.dataset.moveKey;
    selectedMoveAxis = hit.dataset.moveAxis || "";
    canvasFocused = true;
    hit.closest(".figure-canvas").focus({preventScroll:true});
    highlight();
  };
  document.addEventListener("DOMContentLoaded", () => {
    Shiny.addCustomMessageHandler("clear-figure-selection", _message => {
      selected = "";
      selectedMark = "";
      selectedMove = "";
      selectedMoveAxis = "";
      canvasFocused = false;
      highlight();
      inspect("");
    });
  });
  document.addEventListener("change", e => {
    if (e.target.name === "tile_scope") highlight();
  });
  document.addEventListener("focusin", e => {
    if (e.target.matches(".figure-canvas")) {
      canvasFocused = Boolean(selectedMove);
      return;
    }
    canvasFocused = e.target.matches(".figure-tick, .figure-axis");
    if (canvasFocused) select(e.target);
  });
  // Keep a single batch in flight. Each key repeat remains a distinct undo step.
  const pending = [];
  let inFlight = false;
  let timer = null;
  let registered = false;
  let deferredButton = null;
  function flush() {
    timer = null;
    if (inFlight || !pending.length) return;
    inFlight = true;
    Shiny.setInputValue("movements", JSON.stringify(pending.splice(0)), {priority:"event"});
  }
  function enqueue(action) {
    if (deferredButton) return;
    if (!registered) {
      Shiny.addCustomMessageHandler("movements-applied", acknowledgement => {
        inFlight = false;
        if (pending.length) flush();
        else if (deferredButton) {
          const id = deferredButton;
          deferredButton = null;
          document.getElementById(id)?.click();
        }
      });
      registered = true;
    }
    pending.push(action);
    if (!inFlight && timer === null) timer = setTimeout(flush, 30);
  }
  const send = (key, dx, dy) => enqueue({key, dx, dy});
  document.addEventListener("click", e => {
    const button = e.target.closest("#done, #delete_shape, #apply_layer, #open_example, #restore_original, #edit_composition_plot, #apply_composition_layout");
    if (!button || (!inFlight && !pending.length)) return;
    if (button.id === "apply_layer" && document.getElementById("layer_operation").value !== "delete") return;
    e.preventDefault();
    e.stopImmediatePropagation();
    deferredButton = button.id;
  }, true);
  document.addEventListener("pointerdown", e => {
    if (e.button !== 0 || drag) return;
    const target = e.target;
    if (target.matches(".figure-axis")) select(target);
    const shape = target.matches(".figure-mark-hit[data-move-key]:not([data-move-axis])");
    if (!shape && !target.matches(".figure-tick")) return;
    target.setPointerCapture(e.pointerId);
    const canvas = target.closest(".figure-canvas");
    drag = {target, canvas, shape, x: e.clientX, y: e.clientY, id: e.pointerId};
    if (shape) {
      e.preventDefault();
      selectMovableMark(target);
    } else {
      target.focus();
      select(target);
    }
  });
  const dragDelta = (drag, e) => {
    const rect = drag.canvas.getBoundingClientRect();
    return [(e.clientX - drag.x) * drag.canvas.dataset.width / rect.width,
      (drag.y - e.clientY) * drag.canvas.dataset.height / rect.height];
  };
  document.addEventListener("pointermove", e => {
    if (!drag || drag.id !== e.pointerId) return;
    if (drag.shape) {
      const [dx, dy] = dragDelta(drag, e);
      drag.canvas.querySelector(".figure-mark-selection").setAttribute("transform", `translate(${dx} ${-dy})`);
    } else {
      drag.target.style.transform = `translate(${e.clientX - drag.x}px,${e.clientY - drag.y}px)`;
    }
  });
  const finish = e => {
    if (!drag || drag.id !== e.pointerId) return;
    const action = drag;
    drag = null;
    const {target, canvas, shape, x, y} = action;
    if (shape) canvas.querySelector(".figure-mark-selection").removeAttribute("transform");
    else target.style.transform = "";
    if (e.type !== "pointerup") return;
    if (shape) inspect(target.dataset.inspect);
    if (Math.hypot(e.clientX - x, e.clientY - y) < 3) {
      if (!shape) inspect(target.dataset.inspectMark || target.dataset.key);
      return;
    }
    send(shape ? target.dataset.moveKey : target.dataset.key, ...dragDelta(action, e));
  };
  document.addEventListener("click", e => {
    if (e.target.matches(".figure-mark-hit")) {
      if (e.target.dataset.moveKey) {
        if (e.target.dataset.moveAxis) {
          selectMovableMark(e.target);
          inspect(selectedMark);
        }
        return;
      }
      selected = "";
      selectedMark = e.target.dataset.inspect;
      selectedMove = "";
      selectedMoveAxis = "";
      canvasFocused = false;
      highlight();
      inspect(selectedMark);
      return;
    }
    if (e.target.matches(".figure-axis") || (e.detail === 0 && e.target.matches(".figure-tick"))) {
      e.target.focus({preventScroll:true});
      select(e.target);
      inspect(e.target.dataset.inspectMark || e.target.dataset.key);
    }
  });
  document.addEventListener("pointerup", finish);
  document.addEventListener("pointercancel", finish);
  document.addEventListener("lostpointercapture", finish);
  document.addEventListener("keydown", e => {
    if ((e.ctrlKey || e.metaKey) && !e.shiftKey && !e.altKey && e.key.toLowerCase() === "z") {
      if (e.target.isContentEditable || e.target.closest("textarea, input, select")) return;
      e.preventDefault();
      enqueue({undo:true});
      return;
    }
    if (!canvasFocused || (e.target !== document.body &&
      !e.target.matches(".figure-tick, .figure-axis, .figure-canvas"))) return;
    const step = document.getElementById("nudge").valueAsNumber;
    if (!Number.isFinite(step) || step <= 0) return;
    const delta = {ArrowLeft:[-step,0], ArrowRight:[step,0], ArrowUp:[0,step], ArrowDown:[0,-step]}[e.key];
    if (delta) {
      const button = buttons().find(item => item.dataset.key === selected);
      const key = selectedMove || button?.dataset.moveKey;
      if (key) {
        e.preventDefault();
        if ((selectedMoveAxis === "x" && !delta[0]) || (selectedMoveAxis === "y" && !delta[1])) return;
        send(key, ...delta);
      }
    }
  });
  new MutationObserver(() => {
    const canvas = document.querySelector(".figure-canvas");
    const scope = tileScope();
    if (!canvas || (canvas === lastCanvas && scope === lastTileScope)) return;
    lastCanvas = canvas;
    lastTileScope = scope;
    const button = buttons().find(item => item.dataset.key === selected);
    if (!button && !selectedMove) { selected = ""; canvasFocused = false; }
    highlight();
    if (canvasFocused && document.activeElement === document.body)
      (selectedMove ? canvas : button)?.focus({preventScroll:true});
  }).observe(document.documentElement, {subtree: true, childList: true});
})();
