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
  let imgs = document.getElementsByClassName(x).getElementsByTagName('img');

  zb = this;
  this.tarClass = x;
  this.container = document.getElememtById('bg-zoom-img-container');

  const zbTpl = document.GetElementById('bgw-zoom-img-template');

  for(let i in imgs) {
    if(typeof(imgs[i].getAttribute('data-src-big') != 'string') {
      throw new Error("ZoomImages error: input lacking a srcBig attribute.");
    }
    /* Initialize the modals */

    const zbNew = zbTpl.cloneNode(true)
    zbNew.id = "";
    zbNew.children()[1].src = imgs[i].getAttribute('data-src-big');
    imgs[i].setAttribute('data-big-id', i);
    imgs[i].addEventListener("mouseup", function(e) {zb.showModal(e)});
    this.container.append(zbNew);
  }
}
/*
 */
BgwZoomImages.prototype.showModal = function(e) {
  const img = e.target;

  if(img.class != this.tarClass) {
    throw new Error("ZoomImage internal error: unexpected class for object.");
  }
  if(img.tagName != 'IMG') {
    throw new Error("ZoomImage internal error: unexpected tag name for object.");
  }
  if(typeof(img.getAttribute('data-src-big') != 'string')) {
    throw new Error("ZoomImage error: data-src-big attribute not string.");
  }

  const zbImg = zbNew.children()[0];
  const zbCaption = '';


  // // Get coordinates for when we do smooth transition
  // const imgCoord = img.getBoundingClientRect();


  // Get target element
  // Read srcBig attribute
  // create image
  //   make with no display
  //   out of flow
  // Animate image
}
