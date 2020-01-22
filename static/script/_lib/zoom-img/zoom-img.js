/*
 * Zoom Images
 *
 * TODO:
 *
 * * Nice smooth transition from existing image to modal
 * * Add ability to cycle through all zoomable images
 * * Add second level zoom
 * * Add capability to recover caption from original image
 */

function BgwZoomImages(x) {
  if(typeof(x) != 'string') {
    throw new Error("flipbook error: input is not an object");
  }
  let imgs = document.querySelectorAll('img.' + x);

  this.tarClass = x;
  var zb = this;

  this.container = document.getElementById('bgw-zoom-img-container');
  if(this.container == null) {
    throw new Error("ZoomImages error: image container not found.");
  }
  const zbTpl = document.getElementById('bgw-zoom-img-template');
  if(zbTpl == null) {
    throw new Error("ZoomImages error: image template not found.");
  }
  for(let i = 0; i < imgs.length; ++i) {
    if(typeof(imgs[i].getAttribute('data-src-big')) != 'string') {
      throw new Error("ZoomImages error: input lacking a srcBig attribute.");
    }
    /* Initialize the modals */

    const zbNew = zbTpl.cloneNode(true);
    zbNew.id = "";
    const zbFig = zbNew.children[0];
    const zbClose = zbFig.children[0];
    const zbImg = zbFig.children[1];
    const zbCapt = zbFig.children[2];
    zbImg.src = imgs[i].getAttribute('data-src-big');

    if(typeof(zbImg.src) != 'string') {
      throw new
        Error("ZoomImages error: missing big image attribute for img " + i);
    }
    imgs[i].setAttribute('data-big-id', i);
    imgs[i].addEventListener("mouseup", function(e) {zb.showModal(e)});
    zbClose.addEventListener("mouseup", function(e) {zb.closeModal(e)});
    this.container.append(zbNew);
  }
}
/*
 */
BgwZoomImages.prototype.showModal = function(e) {
  const img = e.target;
  const imgCont = document.getElementById('bgw-zoom-img-container').children
  const imgBig = imgCont[img.getAttribute('data-big-id')]
  imgBig.style.display='inline-block';

  // // Get coordinates for when we do smooth transition
  // const imgCoord = img.getBoundingClientRect();
}
BgwZoomImages.prototype.closeModal = function(e) {
  e.target.parentElement.parentElement.style.display = 'none';
}
// zoom-imageize everything with class bgw-zoom-img

new BgwZoomImages('bgw-zoom-img');

