/* www/app.js
   - Fullscreen toggles for elements with .fullscreen-toggle
*/

function requestFs(el) {
  if (!el) return;

  // already fullscreen? -> exit
  if (document.fullscreenElement || document.webkitFullscreenElement) {
    if (document.exitFullscreen) return document.exitFullscreen();
    if (document.webkitExitFullscreen) return document.webkitExitFullscreen();
    return;
  }

  // enter fullscreen (support Safari prefix)
  if (el.requestFullscreen) return el.requestFullscreen({ navigationUI: "hide" });
  if (el.webkitRequestFullscreen) return el.webkitRequestFullscreen();
}

(function initFullscreenDelegation() {
  document.addEventListener("click", function (e) {
    const interactive = e.target.closest("a, button, input, textarea, select, label");
    if (interactive) return;

    const box = e.target.closest(".fullscreen-toggle");
    if (!box) return;

    requestFs(box);
  });

  document.addEventListener("keydown", function (e) {
    if (e.key !== "Escape") return;
    if (document.fullscreenElement && document.exitFullscreen) document.exitFullscreen();
    if (document.webkitFullscreenElement && document.webkitExitFullscreen) document.webkitExitFullscreen();
  });
})();
