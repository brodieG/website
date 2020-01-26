/*
Zoom Images

Suggested usage is adding

  ```{r child='../../static/script/_lib/zoom-img/zoom-img.Rmd', results='asis'}
  ```

To a post.  Automatically all IMG elements with class 'bgw-zoom-img' and a
'data-src-big' attribute with the location of the large image will be made
zoomable.  Optionally setting a 'data-caption' attribute will use that as the caption for zoomed in image, otherwise if image is inside FIGURE element the FIGURE element caption will be used.

TODO:

* Nice smooth transition from existing image to modal
* Add ability to cycle through all zoomable images
* Add second level zoom
*/

function BgwZoomImages(x) {
  if(typeof(x) != 'string') {
    throw new Error("flipbook error: input is not an object");
  }
  let imgs = document.querySelectorAll('img.' + x);

  this.tarClass = x;
  this.activeEl = null;
  var zb = this;

  this.container = document.getElementById('bgw-zoom-img-container');
  if(this.container == null) {
    throw new Error("ZoomImages error: image container not found.");
  }
  if(this.container.children.length) {
    throw new Error(
      "ZoomImages error: container already instantiated, should only be one " +
      "of them per page."
    );
  }
  const zbFrameTpl = document.getElementById('bgw-zoom-frame-template')
  if(zbFrameTpl == null) {
    throw new Error("ZoomImages error: zoom frame template not found.");
  }
  for(let i = 0; i < imgs.length; ++i) {
    if(typeof(imgs[i].getAttribute('data-src-big')) != 'string') {
      throw new Error("ZoomImages error: input lacking a srcBig attribute.");
    }
    /* Check what type we're dealing with */

    let dataCapt = imgs[i].getAttribute('data-caption');

    const zbNew = zbFrameTpl.cloneNode(true);
    zbNew.id = "";
    const zbContent = zbNew.getElementsByTagName('FIGURE')[0];
    const zbInner = zbNew.getElementsByClassName('bgw-zoom-border')[0];

    let figCapt = '';
    const figCapts = imgs[i].parentElement.getElementsByTagName('figcaption');
    if(typeof(dataCapt) == 'string') {
      figCapt = dataCapt;
    } else if (figCapts.length) {
      figCapt = figCapts[0].innerHTML;
    }
    const zbCaptEl = zbContent.getElementsByTagName('FIGCAPTION')[0];
    if(figCapt.length) {
      zbCaptEl.innerHTML = figCapt;
      zbNew.setAttribute(
        'class', zbNew.getAttribute('class') + ' bgw-with-caption'
      );
    } else {
      zbCaptEl.style.display = 'none'
    }
    const zbClose = zbNew.getElementsByClassName('bgw-zoom-boxclose')[0];
    const zbImg = zbNew.getElementsByTagName('IMG')[0];

    zbImg.src = imgs[i].getAttribute('data-src-big');

    if(typeof(zbImg.src) != 'string') {
      throw new
        Error("ZoomImages error: missing big image attribute for img " + i);
    }
    imgs[i].setAttribute('data-big-id', i);
    imgs[i].addEventListener("mouseup", function(e) {zb.showModal(e)});
    zbClose.addEventListener("mouseup", function(e) {zb.closeModal(e)});
    zbNew.addEventListener("mouseup", function(e) {zb.closeModal(e)});
    zbContent.addEventListener("mouseup", function(e) {e.stopPropagation();});
    document.addEventListener("keyup", function(e) {
      if(zb.activeEl != null) {
        zb.activeEl.style.display = 'none';
      }
      zb.activeEl = null;
    });
    this.container.appendChild(zbNew);
  }
}
/*
 */
BgwZoomImages.prototype.showModal = function(e) {
  if(e.button == 0) {
    const img = e.target;
    const imgCont = document.getElementById('bgw-zoom-img-container').children
    const imgBig = imgCont[img.getAttribute('data-big-id')]
    imgBig.style.display='inline-block';
    this.activeEl = imgBig;
  }

  // // Get coordinates for when we do smooth transition
  // const imgCoord = img.getBoundingClientRect();
}
/*
 * Handle closing of modal.  Super janky, need to cleanup some day (yeah right).
 *
 * Only for mouse events
 */
BgwZoomImages.prototype.closeModal = function(e) {
  if(this.activeEl != null && e.button == 0) {
    // for some inscrutable reason I was getting duplicate mouseup events, but
    // only with img, not fig templates, and only when clicking on closing box,
    // not on main div
    this.activeEl.style.display = 'none'
    this.activeEl = null;
  }
}
// zoom-imageize everything with class bgw-zoom-img

new BgwZoomImages('bgw-zoom-img');

