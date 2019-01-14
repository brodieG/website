/*
 * Copyright (C) 2019 Brodie Gaslam
 *
 * This file is part of the website "www.brodieg.com"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
 */
// - Globals -------------------------------------------------------------------

var playing = false;
var i_width=0;
var i_height=0;
var img_dir =
  '/post/2019-01-11-reverse-polish-notation-parsing-in-r_files/flipbook';
var img_n = 0;
var pad = "00";
var img_active = 0;
var fps_def = 3;
var fps_last = fps_def;
var interval;
var playing = false;

var flipbook = document.getElementById('bg-rpn-flipbook');
var imgs = document.getElementById('bg-rpn-images');
var play = document.getElementById('bg-rpn-play');
var stepf = document.getElementById('bg-rpn-step-f');
var stepb = document.getElementById('bg-rpn-step-b');
var fps = document.getElementById('bg-rpn-fps');
var frame = document.getElementById('bg-rpn-frame');
var stop = document.getElementById('bg-rpn-stop');
var frame_n = document.getElementById('bg-rpn-frame-n');

var i_width = flipbook.width;
var i_height = flipbook.height;

if(!isNaN(parseInt(frame.value))) {img_active = parseInt(frame.value);};
if(!isNaN(parseInt(frame_n.innerHTML))) {img_n = parseInt(frame_n.innerHTML)};

// - Load Images ---------------------------------------------------------------

for(i = 0; i <= img_n; i++) {
  var img = document.createElement("img");
  var img_n_str = "" + i;
  var img_file = pad.substring(0, pad.length - img_n_str.length) + img_n_str;
  var img_src = img_dir + '/img-' + img_file + '.png'

  img.src = img_src;
  imgs.append(img);
}
// - Funs ----------------------------------------------------------------------

function fps_read() {
  var val = parseFloat(fps.value);
  if(isNaN(val) || val < 0) val = fps_last;
  fps_last = val;
  return val;
}
interval = 1/fps_read() * 1000;

var ctx = flipbook.getContext("2d");
var intervalID = 0;

function draw() {
  frame.value = img_active;
  ctx.drawImage(imgs.children[img_active], 0, 0, i_width, i_height);
}
function pause_flip() {
  playing = false;
  clearInterval(intervalID);
}
function step_img(val) {
  img_active = img_active + val;
  if(img_active > img_n) {img_active = 1;}
  if(img_active < 0) {img_active = img_n;}
  draw();
}
function step_click(e) {
  pause_flip();
  if(e.shiftKey) {step_img(-1);} else {step_img(1);}
}
function step() {
  if(img_active == img_n - 1) {
    // delay at end for four frames
    step_img(1);
    pause_flip();
    setTimeout(play_all, 10 * interval);
  } else {
    step_img(1);
  }
}
function play_all() {
  if(playing) {
    pause_flip();
    return null;
  }
  clearInterval(intervalID);
  intervalID = setInterval(step, interval);
  playing = true;
}
function step_f() {
  pause_flip();
  if(img_active == img_n) {img_active = 0} else {img_active += 1;}
  draw();
};
function step_b() {
  pause_flip();
  if(img_active == 0) {img_active = img_n} else {img_active -= 1;}
  draw();
};
// - Handlers ------------------------------------------------------------------

flipbook.addEventListener("click", step_click);
stepf.addEventListener("mouseup", step_f);
stepb.addEventListener("mouseup", step_b);
play.addEventListener("mouseup", play_all);
stop.addEventListener("mouseup", function(e) {
  pause_flip();
  img_active = 0;
  draw();
});
// FPS
fps.addEventListener("input", function(e) {
  interval = 1/fps_read() * 1000;
  pause_flip();
  play_all();
});
// Frame
frame.addEventListener("input", function(e) {
  var frame_val = parseInt(frame.value)
  if(
    (isNaN(frame_val) || frame_val < 0 || frame_val > img_n) ||
    frame_val == img_active
  ) {return;}
  img_active = frame_val;
  pause_flip();
  draw();
});

window.addEventListener("load", function() {draw();});
