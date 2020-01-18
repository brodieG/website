/*
Instantiate a Flipbook Object

A flipbook will display images from a directory onto a canvas 2D object.  This
javascript object relies on a pre-existing flipbook template loaded into the
document which should be available in the companion 'flipbook.html' file.  The
user is responsible for injecting that HTML into the same page as this JS.

Suggested usage is something like:

    A flipbook goes here:
    <div id='flipbook1'></div>

    ```{r child='../../static/script/_lib/flipbook/flipbook.Rmd', results='asis'}
    ```
    <script type='text/javascript'>
    new BgFlipBook({
      targetId: 'flipbook1',
      imgDir: '/post/2019-08-23-mesh-reduction-1_files/images/flipbook/',
      imgStart: 7, imgEnd: 53,  imgPad: "0000",
      fps: 4, loop: true, loopDelay: 8
    })
    </script>

The Rmd combines the JS and HTML inclusion.  It seems best to include this code
after the target divs, although only the `new BgFlipBook` calls should need
to happen after they are defined, in theory (failed in practice).

Failure to set the directory or file names properly will result in errors like
"NS_ERROR_NOT_AVAILABLE" (firefox).

naturalWidth and naturalHeight of the first image will set the size of the HTML
canvas element the images are drawn in.

Flipbook is configured via a single object with named values.  Here "@param x"
should be taken to mean "obj.x"
@param targetId string id of pre-existing DIV that will be populated
  with the flipbook.  The DIV must be empty.
@param imgDir string location for images for flipbook, the images must be named
  in format img-001.png (see `pad` as well).
@param imgEnd where to end the flipbook, required because we cannot get a
  directory listing so don't know how many images there are.
@param imgStart positive integer where to start the flipbook, must be less than
  imgEnd
@param fps frame rate in frames per second.
@param loop boolean whether to loop back to beginning when auto-playing.
@param loopDelay number how many frames to pause when playing before looping
  back to start.
@param imgPad string of form "0", "00", "000", etc., of length corresponding to
  how many digits re used in the image file names.
@param helpFillStyle background color for help pop-up box, may need to be
  adjusted if flipbook contents are mostly dark.
@param helpTextStyle background color for help pop-up box, may need to be
  adjusted if flipbook contents are mostly dark.
@param width string a width string like "100%" or "800px" to use as the CSS
  'width' value for the container.
@return an instantiated flipboook object, although it serves no real purpose as
  the constructor attaches all the event handlers and nothing else beyond that
  is needed.

TODO: provide a way to pass CSS for the whole canvas and container div.
*/

/*---------------------------------------------------------------------------*\
 * Constructor ***************************************************************|
\*---------------------------------------------------------------------------*/

const bgFlipBookDebug = false;
function BgFlipBook(x) {
  // - Validate ----------------------------------------------------------------

  if(typeof(x) != 'object') {
    throw new Error("flipbook error: input is not an object");
  }
  // Defaults
  xDef = {
    targetId: null,
    imgDir: null,
    imgEnd: null,
    imgStart: 1,
    imgPad: "000",
    fps: 1,
    loopDelay: 2,
    loop: true,
    helpFillStyle: 'rgb(0, 0, 0, .7)',
    helpTextStyle: 'white',
    width: 'auto'
  }
  for (let k in x) {
    if (x.hasOwnProperty(k)) {
      if(typeof(xDef[k]) == 'undefined') {
        throw new Error(
          "flipbook error: " + k +" is not a known property"
        );
      }
      xDef[k] = x[k]
  } }
  for (let k in xDef) {
    if (xDef.hasOwnProperty(k)) {
      if(xDef[k] == null) {
        throw new Error(
          "flipbook error: required property " + k +" was no provided"
        );
  } } }
  x = xDef;

  if(typeof(x.targetId) != "string") {
    throw new Error("flipbook error: 'targetId' is not a string");
  }
  if(typeof(x.imgDir) != "string") {
    throw new Error("flipbook error: 'imgDir' is not a string");
  }
  if(typeof(x.imgPad) != "string") {
    throw new Error("flipbook error: 'imgPad' is not a string");
  }
  if(typeof(x.helpFillStyle) != "string") {
    throw new Error("flipbook error: 'helpFillStyle' is not a string");
  }
  if(typeof(x.helpTextStyle) != "string") {
    throw new Error("flipbook error: 'helpTextStyle' is not a string");
  }
  if(
    typeof(x.imgStart) != 'number' || !Number.isInteger(x.imgStart) ||
    x.imgStart < 1
  ) {
    throw new Error("flipbook error: 'imgStart' must be integer > 1.");
  }
  if(
    typeof(x.imgEnd) != 'number' || !Number.isInteger(x.imgEnd) ||
    x.imgEnd < 1
  ) {
    throw new Error("flipbook error: 'imgEnd' must be integer > 1.");
  }
  if(x.imgEnd < x.imgStart) {
    throw new Error("flipbook error: 'imgEnd' must be GTE to 'imgStart'.");
  }
  if(typeof(x.fps) != 'number' || x.fps <= 0) {
    throw new Error("flipbook error: 'fps' must be numeric > 0.");
  }
  if(typeof(x.loopDelay) != 'number' || x.loopDelay < 0) {
    throw new Error("flipbook error: 'loopDelay' must be numeric >= 0.");
  }
  if(typeof(x.loop) != 'boolean') {
    throw new Error("flipbook error: 'loop' must be boolean.");
  }
  if(typeof(x.width) != "string") {
    throw new Error("flipbook error: 'width' is not a string");
  }
  // Find target and template DOM objects

  const target = document.getElementById(x.targetId)
  if(target == null) {
    throw new Error(
      "flipbook error: could not find target div with id '" + x.targetId + "'."
    );
  }
  if(target.childElemCount) {
    throw new Error("flipbook error: target div is not empty.");
  }
  const flipTpl = document.getElementById('bg-flipbook-template');
  if(flipTpl == null) {
    throw new Error("flipbook error: could not find flipbook template.");
  }
  // - Init --------------------------------------------------------------------

  // Clone the object, generate handles for all the sub-elements, and
  // remove ids to avoid id conflicts with the template when we instert in DOM

  const flipNew = flipTpl.cloneNode(true)

  this.els = {
    container: flipNew.querySelector('#bg-flipbook-container'),
    flipbook: flipNew.querySelector('#bg-flipbook-flipbook'),
    imgs: flipNew.querySelector('#bg-flipbook-images'),
    play: flipNew.querySelector('#bg-flipbook-play'),
    help: flipNew.querySelector('#bg-flipbook-help'),
    stepf: flipNew.querySelector('#bg-flipbook-step-f'),
    stepb: flipNew.querySelector('#bg-flipbook-step-b'),
    fps: flipNew.querySelector('#bg-flipbook-fps'),
    frame: flipNew.querySelector('#bg-flipbook-frame'),
    stop: flipNew.querySelector('#bg-flipbook-stop'),
    frameN: flipNew.querySelector('#bg-flipbook-frame-n'),
    loop: flipNew.querySelector('#bg-flipbook-loop'),
    frameSpan: flipNew.querySelector('#bg-flipbook-frame-span')
  }
  for(let i in this.els) {
    if(this.els[i] == null) {
      throw new Error("flipbook error: template missing '" + i + "' element.");
    }
    this.els[i].id = ""
  }
  this.ctx = this.els.flipbook.getContext("2d");
  if(!this.ctx) {
    throw new Error("flipbook error: failed getting canvas 2d context");
  }
  // Other properties

  this.imgN = x.imgEnd - x.imgStart + 1;
  this.playing = false;
  this.imgPad = x.imgPad;
  this.imgActive = 1;
  this.fpsLast = x.fps;
  this.loopDelay = x.loopDelay;
  this.init = false;
  this.helpActive = false;
  this.helpFillStyle = x.helpFillStyle;
  this.helpTextStyle = x.helpTextStyle;
  this.intervalID = 0;
  this.width = x.width;
  this.playX = 0;
  this.playY = 0;
  this.playR = 0;

  // Initialize HTML els

  this.els.frameN.innerHTML = this.imgN;
  this.els.fps.value = this.fpsLast;
  this.interval = 1 / this.fpsRead() * 1000;
  if(x.loop) {
    this.els.loop.checked=true;
  }
  if(!isNaN(parseInt(this.els.frame.value))) {
    this.imgActive = parseInt(this.els.frame.value);
  };

  // - Load Images -------------------------------------------------------------

  for(i = x.imgStart; i <= x.imgEnd; ++i) {
    const img = document.createElement("img");
    const imgNStr = "" + i;
    const imgFile =
      this.imgPad.substring(0, this.imgPad.length - imgNStr.length) + imgNStr;
    const imgSrc = x.imgDir + '/img-' + imgFile + '.png'
    img.src = imgSrc;
    this.els.imgs.append(img);
  }
  // - Insert Into DOM ---------------------------------------------------------

  while(flipNew.hasChildNodes()) {
    target.appendChild(flipNew.firstChild);
  }
  // - Register Handlers -------------------------------------------------------

  var flip = this;
  this.els.flipbook.addEventListener("click", function(e) {flip.stepClick(e)});
  this.els.stepf.addEventListener("mouseup", function() {flip.stepF()});
  this.els.stepb.addEventListener("mouseup", function() {flip.stepB()});
  this.els.play.addEventListener("mouseup", function() {flip.playAll()});
  this.els.help.addEventListener("mouseup", function() {flip.drawHelp()});
  this.els.stop.addEventListener("mouseup", function() {flip.handleStop()});
  this.els.fps.addEventListener("input", function() {flip.handleInputFPS()});
  this.els.frame.addEventListener("input", function() {flip.handleInputFrame()});;
  window.addEventListener("load", function() {flip.handleLoad()});
}
/*---------------------------------------------------------------------------*\
 * Methods *******************************************************************|
\*---------------------------------------------------------------------------*/

BgFlipBook.prototype.draw = function() {
  this.els.frame.value = this.imgActive;
  if(!this.init) {
    this.els.container.style.width = this.width;
    this.els.flipbook.style.width = this.width;

    this.els.flipbook.width = this.els.imgs.children[0].naturalWidth;
    this.els.flipbook.height = this.els.imgs.children[0].naturalHeight;
  }
  this.ctx.drawImage(
    this.els.imgs.children[this.imgActive - 1], 0, 0,
    this.els.flipbook.width, this.els.flipbook.height
  );
}
BgFlipBook.prototype.drawHelp = function() {
  if(bgFlipBookDebug) {console.log('Draw Help')};
  const fontSize = this.els.flipbook.width / 25;
  this.pauseFlip();
  this.draw();
  const text = [
    "        \u{2022} Click in frame to step forward",
    "        \u{2022} Shift + click to step backwards",
    "        \u{2022} Use the controls below"
  ]
  /* figure out center point to put the text in */

  this.ctx.font = fontSize + 'px serif';
  const th = this.ctx.measureText('M').width * 1.1;

  let textMaxWidth = 0;
  for(let i = 0; i < text.length; i++) {
    if(textMaxWidth < this.ctx.measureText(text[i]).width) {
      textMaxWidth = this.ctx.measureText(text[i]).width;
    }
  }
  /* not guarnteed to fit b/c pad */
  const pad = th * .4;
  const textOff = .3;
  const textExtra = textOff * textMaxWidth;
  const totalWidth = textMaxWidth + textExtra + 4 * pad;
  const textTotHeight = th * text.length;
  const xstart = (this.els.flipbook.width - totalWidth) / 2;
  const ystart = (this.els.flipbook.height - textTotHeight) / 2 + th;
  this.playR = pad * 4;
  this.playX = xstart + textExtra / 2 + pad;
  this.playY = ystart + textTotHeight / 2 - 2 * pad;

  /* play button */

  this.ctx.fillStyle = this.helpFillStyle;
  this.ctx.beginPath();
  this.ctx.arc(this.playX, this.playY, (textTotHeight / 2 + pad), 0, 2*Math.PI);
  this.ctx.fill();

  this.ctx.fillStyle = this.helpTextStyle;
  this.ctx.beginPath();
  this.ctx.moveTo(this.playX - this.playR / 3, this.playY - this.playR / 2);
  this.ctx.lineTo(this.playX - this.playR / 3, this.playY + this.playR / 2);
  this.ctx.lineTo(this.playX + this.playR * 2 / 3, this.playY);
  this.ctx.fill()

  /* pre-draw the marquee the text will overlay on */

  this.ctx.font = fontSize + 'px serif';
  this.ctx.fillStyle = this.helpFillStyle;
  this.ctx.roundRect(
    xstart + textExtra + 2 * pad, ystart - 3 * pad,
    textMaxWidth + pad * 2, textTotHeight + 2 * pad, th / 2
  ).fill();
  this.ctx.fillStyle = this.helpTextStyle;
  for(i = 0; i < text.length; i++) {
    this.ctx.fillText(text[i], xstart + textExtra + 3 * pad, ystart + th * i);
  }
  this.ctx.fillText("OR", xstart + textExtra + 3 * pad, ystart + th);

  this.helpActive = true;
}
BgFlipBook.prototype.pauseFlip = function() {
  //if(bgFlipBookDebug) {console.log('pause clear interval')};
  this.playing = false;
  clearInterval(this.intervalID);
}
BgFlipBook.prototype.stepFInt = function() {
  if(bgFlipBookDebug) {
    console.log('StepF imgActive ' + this.imgActive + ' imgN ' + this.imgN)
  };
  if(this.imgActive == this.imgN) {
    if(bgFlipBookDebug) {console.log('StepF reset img')};
    var oldStyle = this.els.frameSpan.style.backgroundColor;
    this.els.frameSpan.style.backgroundColor = 'yellow';
    var flip = this;
    setTimeout(
      function() {
        flip.els.frameSpan.style.backgroundColor=oldStyle;
      },
      200
    );
    if(this.els.loop.checked) {this.imgActive = 1;}
  } else {
    this.imgActive += 1;
  }
}
BgFlipBook.prototype.stepBInt = function() {
  if(this.imgActive == 1) {
    var oldStyle = this.els.frameSpan.style.backgroundColor;
    this.els.frameSpan.style.backgroundColor = 'yellow';
    var flip = this;
    setTimeout(
      function() {
        flip.els.frameSpan.style.backgroundColor=oldStyle;
      },
      200
    );
    if(this.els.loop.checked) {this.imgActive = this.imgN;}
  } else {
    this.imgActive -= 1;
  }
};
BgFlipBook.prototype.changeFrame = function(dir) {
  if(bgFlipBookDebug) {
    console.log('change frame ' + dir + ' help act ' + this.helpActive)
  };
  if(!this.helpActive) {
    if(dir > 0) this.stepFInt(); else this.stepBInt();
    this.draw();
  } else if(this.helpActive) {
    this.helpClear();
  }
}
BgFlipBook.prototype.step = function(dir) {
  this.pauseFlip();
  this.changeFrame(dir);
}
BgFlipBook.prototype.stepF = function() {this.step(1);}
BgFlipBook.prototype.stepB = function() {this.step(-1);}
BgFlipBook.prototype.stepClick = function(e) {
  this.pauseFlip();
  const mouse = getCanvasMousePos(this.els.flipbook, e);
  const dist = Math.sqrt((mouse.X - this.playX)^2 + (mouse.Y - this.playY)^2);
  if(this.helpActive && dist <= this.playR) {
    this.playAll();
  } else {
    if(e.shiftKey) {this.stepB();} else {this.stepF();}
  }
}
// automated stepping, pauses at end
BgFlipBook.prototype.stepAuto = function() {
  if(bgFlipBookDebug) {console.log('stepping ', this.imgActive)};
  if(this.imgActive == this.imgN) {
    if(this.els.loop.checked) {
      // delay at end
      this.pauseFlip();
      var flip = this;
      setTimeout(
        function() {
          if(bgFlipBookDebug) {console.log('end image')};
          flip.changeFrame(1);
          flip.pauseFlip();
          flip.resumeAll();
        },
        this.loopDelay * this.interval
      );
    } else {
      this.pauseFlip();
    }
  } else {
    this.changeFrame(1);
  }
}
BgFlipBook.prototype.playAll = function() {
  if(this.playing) {
    this.pauseFlip();
    return null;
  }
  clearInterval(this.intervalID);
  this.stepF();  // always immediately advance
  var flip = this;
  this.intervalID = setInterval(function() {flip.stepAuto()}, this.interval);
  if(bgFlipBookDebug) {console.log('Interval ID set to ' + this.intervalID)}
  this.playing = true;
}
/*
Restart when looping
*/
BgFlipBook.prototype.resumeAll = function() {
  clearInterval(this.intervalID);
  var flip = this;
  if(bgFlipBookDebug) {console.log('Setting step with int ' + this.interval)}
  this.intervalID = setInterval(function(){flip.stepAuto()}, flip.interval);
  if(bgFlipBookDebug) {console.log('Interval ID set to ' + this.intervalID)}
  this.playing = true;
}
BgFlipBook.prototype.helpClear = function() {
  if(this.helpActive) {
    this.helpActive = false;
    this.draw();
  }
}
BgFlipBook.prototype.fpsRead = function() {
  let val = parseFloat(this.els.fps.value);
  if(isNaN(val) || val < 0) val = this.fpsLast;
  this.fpsLast = val;
  return val;
}
// - Handler Funs --------------------------------------------------------------

BgFlipBook.prototype.handleStop = function() {
  this.pauseFlip();
  this.imgActive = 1;
  this.draw();
};
BgFlipBook.prototype.handleInputFPS = function() {
  this.interval = 1/this.fpsRead() * 1000;
  this.pauseFlip();
  this.playAll();
};
BgFlipBook.prototype.handleInputFrame = function() {
  const frameVal = parseInt(this.els.frame.value)
  if(
    (isNaN(frameVal) || frameVal < 1 || frameVal >= this.imgN) ||
    frameVal == this.imgActive
  ) {return;}
  this.imgActive = frameVal;
  this.pauseFlip();
  this.draw();
};
BgFlipBook.prototype.handleLoad = function() {
  this.draw();
  this.drawHelp();
  this.init=true;
};
/*---------------------------------------------------------------------------*\
 * Utils *********************************************************************|
\*---------------------------------------------------------------------------*/

// Thank you Jhoff and Grumdrig from  https://stackoverflow.com/a/7838871/2725969

CanvasRenderingContext2D.prototype.roundRect = function (x, y, w, h, r) {
  if (w < 2 * r) r = w / 2;
  if (h < 2 * r) r = h / 2;
  this.beginPath();
  this.moveTo(x+r, y);
  this.arcTo(x+w, y,   x+w, y+h, r);
  this.arcTo(x+w, y+h, x,   y+h, r);
  this.arcTo(x,   y+h, x,   y,   r);
  this.arcTo(x,   y,   x+w, y,   r);
  this.closePath();
  return this;
}
// Thank you user1693593 https://stackoverflow.com/a/17130415/2725969

function  getCanvasMousePos(canvas, evt) {
  let rect = canvas.getBoundingClientRect(),
      scaleX = canvas.width / rect.width,
      scaleY = canvas.height / rect.height;
  return {
    x: (evt.clientX - rect.left) * scaleX,
    y: (evt.clientY - rect.top) * scaleY
  }
}
