/*
Instantiate a Flipbook Object

A flipbook will display images from a directory onto a canvas 2D object.  This
javascript object relies on a pre-existing flipbook template loaded into the
document which should be available in the companion 'flipbook.html' file.  The
user is responsible for injecting that HTML into the same page as this JS.  For
an example see 'flipbook.Rmd'.

Failure to set the directory or file names properly will result in errors like
"NS_ERROR_NOT_AVAILABLE" (firefox).

naturalWidth and naturalHeight of the first image will set the size of the HTML
canvas element the images are drawn in.

@param targetId string id of pre-existing DIV that will be populated with the
  flipbook.  The DIV must be empty.
@param imgDir string location for images for flipbook, the images must be named
  in format img-0001.png.
@param imgEnd where to end the flipbook, required because we cannot get a
  directory listing so don't know how many images there are.
@param imgStart where to start the flipbook, must be less than imgEnd
@param fpsInit number the default initial frame rate.
@param endDelay number how many frames to pause when playing before looping
  back to start.
*/

/*---------------------------------------------------------------------------*\
 * Constructor ***************************************************************|
\*---------------------------------------------------------------------------*/

function BgFlipBook(
  targetId, imgDir, imgEnd, imgStart=1, fpsInit=1, endDelay=0
) {
  // - Validate ----------------------------------------------------------------

  // Find target and template DOM objects

  const target = document.getElementById(targetId)
  if(target == null) {
    throw new Error(
      "flipbook error: could not find target div with id '" + targetId + "'."
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
    flipbook: flipNew.querySelector('#bg-flipbook-flipbook'),
    imgs: flipNew.querySelector('#bg-flipbook-images'),
    play: flipNew.querySelector('#bg-flipbook-play'),
    help: flipNew.querySelector('#bg-flipbook-help'),
    stepf: flipNew.querySelector('#bg-flipbook-step-f'),
    stepb: flipNew.querySelector('#bg-flipbook-step-b'),
    fps: flipNew.querySelector('#bg-flipbook-fps'),
    frame: flipNew.querySelector('#bg-flipbook-frame'),
    stop: flipNew.querySelector('#bg-flipbook-stop'),
    frameN: flipNew.querySelector('#bg-flipbook-frame-n')
  }
  for(let i in this.els) {
    if(this.els[i] == null) {
      throw new Error("flipbook error: template missing '", i, "' element.");
    }
    this.els[i].id = ""
  }
  this.ctx = this.els.flipbook.getContext("2d");
  if(!this.ctx) {
    throw new Error("flipbook error: failed getting canvas 2d context");
  }
  // Other properties

  this.imgN = imgEnd - imgStart + 1;
  this.playing = false;
  this.pad = "000";
  this.imgActive = 1;
  this.fpsLast = fpsInit;
  this.endDelay = endDelay;
  this.init = false;
  this.helpActive = false;
  this.interval = 1 / this.fpsRead() * 1000;
  this.intervalID = 0;

  // Initialize HTML els

  this.els.frameN.innerHTML = this.imgN;
  this.els.fps.value = this.fpsLast;
  if(!isNaN(parseInt(this.els.frame.value))) {
    this.imgActive = parseInt(this.els.frame.value);
  };
  // - Load Images -------------------------------------------------------------

  for(i = imgStart; i <= imgEnd; ++i) {
    const img = document.createElement("img");
    const imgNStr = "" + i;
    const imgFile =
      this.pad.substring(0, this.pad.length - imgNStr.length) + imgNStr;
    const imgSrc = imgDir + '/img-' + imgFile + '.png'
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
    this.els.flipbook.width = this.els.imgs.children[0].naturalWidth;
    this.els.flipbook.height = this.els.imgs.children[0].naturalHeight;
  }
  this.ctx.drawImage(
    this.els.imgs.children[this.imgActive - 1], 0, 0,
    this.els.flipbook.width, this.els.flipbook.height
  );
}
BgFlipBook.prototype.drawHelp = function() {
  console.log('Draw Help');
  const fontSize = this.els.flipbook.width / 25;
  this.pauseFlip();
  this.draw();
  this.ctx.fillStyle = 'rgb(0, 0, 0, .7)';
  this.ctx.fillRect(0, 0, this.els.flipbook.width, this.els.flipbook.height)
  this.ctx.fillStyle = 'white'
  this.ctx.font = fontSize + 'px serif';
  const th = this.ctx.measureText('M').width * 1.1;
  const xoff = this.els.flipbook.width * .1
  const yoff = this.els.flipbook.height * .1
  const text = [
    "This is a flipbook.  You can press '\u25b6' to cycle",
    "through frames, but it is really intended for you to",
    "step through them:",
    "",
    "* Click in frame to step forward",
    "* Shift + click in frame to step backwards",
    "* Or use the controls below"
  ]
  /* figure out center point to put the text in */

  let textMaxWidth = 0;
  for(let i = 0; i < text.length; i++) {
    if(textMaxWidth < this.ctx.measureText(text[i]).width) {
      textMaxWidth = this.ctx.measureText(text[i]).width;
    }
  }
  const textTotHeight = th * text.length;
  const xstart = (this.els.flipbook.width - textMaxWidth) / 2;
  const ystart = (this.els.flipbook.height - textTotHeight) / 2 + th;

  for(i = 0; i < text.length; i++) {
    this.ctx.fillText(text[i], xstart, ystart + th * i);
  }
  this.helpActive = true;
}
BgFlipBook.prototype.pauseFlip = function() {
  //console.log('pause clear interval');
  this.playing = false;
  clearInterval(this.intervalID);
}
BgFlipBook.prototype.stepFInt = function() {
  console.log('StepF imgActive ' + this.imgActive + ' imgN ' + this.imgN);
  if(this.imgActive == this.imgN) {
    console.log('StepF reset img');
    this.imgActive = 1
  } else {
    this.imgActive += 1;
  }
}
BgFlipBook.prototype.stepBInt = function() {
  if(this.imgActive == 1) {
    this.imgActive = this.imgN
  } else {
    this.imgActive -= 1;
  }
};
BgFlipBook.prototype.changeFrame = function(dir) {
  console.log('change frame ' + dir + ' help act ' + this.helpActive);
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
  if(e.shiftKey) {this.stepB();} else {this.stepF();}
}
// automated stepping, pauses at end
BgFlipBook.prototype.stepAuto = function() {
  console.log('stepping ', this.imgActive);
  if(this.imgActive == this.imgN) {
    // delay at end
    this.pauseFlip();
    var flip = this;
    setTimeout(
      function() {
        console.log('end image');
        flip.changeFrame(1);
        flip.pauseFlip();
        flip.resumeAll();
      },
      this.endDelay * this.interval
    );
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
  console.log('Interval ID set to ' + this.intervalID)
  this.playing = true;
}
/*
Restart when looping
*/
BgFlipBook.prototype.resumeAll = function() {
  clearInterval(this.intervalID);
  var flip = this;
  console.log('Setting step with int ' + this.interval);
  this.intervalID = setInterval(function(){flip.stepAuto()}, flip.interval);
  console.log('Interval ID set to ' + this.intervalID)
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

