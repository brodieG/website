# Site Developer Notes

## Overview

These are assorted notes taken during the development of the website that may be
useful in the future.

## Archetypes

* File type of the archetype must match the file type of the post.
* But `blogdown` in `new_post` always uses the ".md" extension irrespective of
  what the provided extension is, and after creating the post renames it.
* Bundle archetypes:
    * For some reason we got this error `Error: stat /Volumes/PERSONAL/repos/website/themes/hugo-nederburg/archetypes/archetypes/post-img/index.md: no such file or directory`.  Notice the repeated 'archetypes' folders.  Moving the bundle into a nested archetype folder fixed this.
    * `hugo_cmd("new post/2019-02-99-yet-another -k post-img")` worked
    * `hugo_cmd("new post/2019-02-99-yet-another.md -k post-img")` did not

Basically we gave up on bundles because trying to get `blogdown` to emit the
correct command just got too complicated.  It seems that the archetype has to
match down to the extension?  We even tried to make a folder with a name like
'post-img.md' but that didn't work.  So we give up on bundled archetypes for
now.

Maybe the problem is that the path is "post" but the kind is "post-img"?

[Allison Hill][2] notes that Academic does have [bundle archetypes][1]:

## Folder Use

The folder static/post/... seem to be recreated during the post generation and
as such are not available and cannot be directly sourced from R code, even
though in other respects the stuff is available (e.g. to link in as HTML).
Ideally we would put all this stuff directly in the content directory but we
have to figure out how to do the folders in the archetype.  Instead, just use
static/... without the post part.

[1]: https://github.com/gcushen/hugo-academic/tree/master/archetypes
[2]: https://twitter.com/apreshill/status/1098696759377780736
