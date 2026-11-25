(() => {
  const root = document.documentElement;
  const preference = "ggfigurestudio-night-mode";
  const readPreference = () => {
    try {
      return (localStorage.getItem(preference) ?? localStorage.getItem("ggfigureedit-night-mode")) === "true";
    } catch {
      return false;
    }
  };
  root.classList.toggle("editor-night", readPreference());

  document.addEventListener("DOMContentLoaded", () => {
    const toggle = document.getElementById("editor-night-mode");
    toggle.checked = root.classList.contains("editor-night");
    toggle.addEventListener("change", () => {
      root.classList.toggle("editor-night", toggle.checked);
      try {
        localStorage.setItem(preference, String(toggle.checked));
      } catch {
        // The switch still works when browser storage is unavailable.
      }
    });
  });
})();
