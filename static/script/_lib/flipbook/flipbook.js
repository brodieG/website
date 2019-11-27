/*
Images must be in `img_dir` and be named in this format:

img-001.png

Failure to set the folder or file names properly will result in errors like
"NS_ERROR_NOT_AVAILABLE" (firefox).

First file is 001, and its naturalWidth and naturalHeight will set the size of
the HTML canvas element the images are drawn in.

@param id string to use as the unique id for this flipbook; it must point to a
  pre-existing div in the DOM that the flipbook will be inserted into.
@param img_dir string location for images for flipbook
@param img_start where to start the flipbook
@param img_end where to end the flipbook, if zero will use all files with
  numbers greater than img_start
 */

var bg_flipbook_init = (
  function() {
    let flipbook_n = 0;
    function(id, img_dir, img_start=1, img_end=0) {
      // check id not already in use

      const target = document.getElementById(id)
      if(target == null) {
        throw new Error(
          "flipbook error: could not find target div with id '" + id + "'"
        );
      }
      // Retrieve template and create new DOM elements

      const flip_tpl = document.getElementById('bg-flipbook-template');
      if(flip_tpl == null) {
        throw new Error("flipbook error: could not find flipbook template.");
      }
      // Clone the object, update all the ids so they are likely unique, and
      // into the placeholder position

      const flip_new = flip_tpl.cloneNode(true)
      const flipbook = flip_new.getElementById('bg-flipbook-flipbook');
      const imgs = flip_new.getElementById('bg-flipbook-images');
      const play = flip_new.getElementById('bg-flipbook-play');
      const help = flip_new.getElementById('bg-flipbook-help');
      const stepf = flip_new.getElementById('bg-flipbook-step-f');
      const stepb = flip_new.getElementById('bg-flipbook-step-b');
      const fps = flip_new.getElementById('bg-flipbook-fps');
      const frame = flip_new.getElementById('bg-flipbook-frame');
      const stop = flip_new.getElementById('bg-flipbook-stop');
      const frame_n = flip_new.getElementById('bg-flipbook-frame-n');

      // clear ids to avoid conflicts with template when we introduce into
      // template; we already have references above

      const flip_elems = flip_new.children;
      for(let i = 0; i < flip_elems.length; ++i) {
        flip_elemns[i].id = ""
      }
      flipbook_n++;

      let playing = false;
      let pad = "000";
      let img_active = 1;
      let fps_last = fps_def;
      let interval;
      let flipbook_init = false;
      let flipbook_help_active = false;

      frame_n.innerHTML = img_n;
      fps.value = fps_last;
      if(!isNaN(parseInt(frame.value))) {img_active = parseInt(frame.value);};

      // - Load Images ---------------------------------------------------------

      for(i = 0; i <= img_n; i++) {
        const img = document.createElement("img");
        const img_n_str = "" + (i + 1);
        const img_file =
          pad.substring(0, pad.length - img_n_str.length) + img_n_str;
        const img_src = img_dir + '/img-' + img_file + '.png'
        img.src = img_src;
        imgs.append(img);
      }
      // - Insert Into DOM -----------------------------------------------------

      while(flip_tpl.hasChildNodes()) {
        target.appendChild(flip_tpl.firstChild);
      }
      // - Funs ----------------------------------------------------------------

      function fps_read() {
        let val = parseFloat(fps.value);
        if(isNaN(val) || val < 0) val = fps_last;
        fps_last = val;
        return val;
      }
      interval = 1/fps_read() * 1000;

      const ctx = flipbook.getContext("2d");
      let intervalID = 0;
      if(ctx) {

        function draw() {
          frame.value = img_active;
          if(!flipbook_init) {
            flipbook.width = imgs.children[0].naturalWidth;
            flipbook.height = imgs.children[0].naturalHeight;
          }
          ctx.drawImage(
            imgs.children[img_active - 1], 0, 0, flipbook.width, flipbook.height
          );
        }
        /*
        Help overlay
        */
        function draw_help() {
          console.log('Draw Help');
          const font_size = flipbook.width / 25;
          pause_flip();
          draw();
          ctx.fillStyle = 'rgb(0, 0, 0, .7)';
          ctx.fillRect(0, 0, flipbook.width, flipbook.height)
          ctx.fillStyle = 'white'
          ctx.font = font_size + 'px serif';
          const th = ctx.measureText('M').width * 1.1;
          const xoff = flipbook.width * .1
          const yoff = flipbook.height * .1
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

          let text_max_width = 0;
          for(let i = 0; i < text.length; i++) {
            if(text_max_width < ctx.measureText(text[i]).width) {
              text_max_width = ctx.measureText(text[i]).width;
            }
          }
          const text_tot_height = th * text.length;
          const xstart = (flipbook.width - text_max_width) / 2;
          const ystart = (flipbook.height - text_tot_height) / 2 + th;

          for(i = 0; i < text.length; i++) {
            ctx.fillText(text[i], xstart, ystart + th * i);
          }
          flipbook_help_active = true;
        }
        function pause_flip() {
          playing = false;
          console.log('pause clear interval');
          clearInterval(intervalID);
        }
        function step_f_int() {
          if(img_active == img_n) {img_active = 1} else {img_active += 1;}
        }
        function step_b_int() {
          if(img_active == 1) {img_active = img_n} else {img_active -= 1;}
        };
        function change_frame(dir) {
          console.log('change frame ' + dir + ' help act ' + flipbook_help_active);
          if(!flipbook_help_active) {
            if(dir > 0) step_f_int(); else step_b_int();
            draw();
          } else if(flipbook_help_active) {
            help_clear();
          }
        }
        function step(dir) {
          pause_flip();
          change_frame(dir);
        }
        function step_f() {step(1);}
        function step_b() {step(-1);}
        function step_click(e) {
          pause_flip();
          if(e.shiftKey) {step_b();} else {step_f();}
        }
        // automated stepping, pauses at end
        function step_auto() {
          console.log('stepping ', img_active);
          if(img_active == img_n) {
            // delay at end
            pause_flip();
            setTimeout(
              function() {
                console.log('end image');
                change_frame(1);
                pause_flip();
                resume_all();
              },
              end_delay * interval
            );
          } else {
            change_frame(1);
          }
        }
        /*
        Intended to be triggered by the play button
        */
        function play_all() {
          if(playing) {
            pause_flip();
            return null;
          }
          clearInterval(intervalID);
          step_f();  // always immediately advance
          intervalID = setInterval(step_auto, interval);
          playing = true;
        }
        /*
        Restart when looping
        */
        function resume_all() {
          clearInterval(intervalID);
          intervalID = setInterval(step_auto, interval);
          playing = true;
        }
        function help_clear() {
          if(flipbook_help_active) {
            flipbook_help_active = false;
            draw();
          }
        }
        // - Handlers ----------------------------------------------------------

        flipbook.addEventListener("click", step_click);
        stepf.addEventListener("mouseup", step_f);
        stepb.addEventListener("mouseup", step_b);
        play.addEventListener("mouseup", play_all);
        help.addEventListener("mouseup", draw_help);
        stop.addEventListener("mouseup", function(e) {
          pause_flip();
          img_active = 1;
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
            (isNaN(frame_val) || frame_val < 1 || frame_val >= img_n) ||
            frame_val == img_active
          ) {return;}
          img_active = frame_val;
          pause_flip();
          draw();
        });
        window.addEventListener("load", 
          function() {
            draw();
            draw_help();
            flipbook_init=true;
          }
        );
      }
    }
  }
)()

// - Globals -------------------------------------------------------------------


