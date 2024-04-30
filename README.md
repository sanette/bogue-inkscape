# Bogue-inkscape

_Use the nice [Inkscape](https://inkscape.org/) drag-and-drop interface to easily create
beautiful [Bogue](https://github.com/sanette/bogue) GUI designs._

## Short description

This library (and command line utility) reads an Inkscape SVG file,
and writes Bogue code automatically that transforms the Inkscape
objects into Bogue widgets. Of course,

+ only specific Inkscape objects are recognized (see below), and
+ the code must then be manually completed to add the logic of your GUI program.

In other words, Inkscape becomes a _helper program_ that you use to
create the Bogue GUI _design_. See examples below.

## Goal

Use Inkscape to help code Bogue designs.

## Non-goals

+ ~~Import any Inkscape file.~~ If you think this will transform any
  Inkscape document into a Bogue GUI, you will be severly
  disappointed.

+ ~~Program the whole GUI within Inkscape.~~ You will only use Inkscape to
  create the GUI _design_, that is, the position and decoration of the
  various widgets. Yes, in fact you can also help creating
  _connections_, but you will still have to program the heart of your
  code by hand.

## Longer description

In order to be correctly recognized, the Inkscape document must be
carefully prepared. Obviously, the *type* of Inkscape objects is of
course important, but not only: we also use the *ID*, the *label*, and
(optionally), the *Description*. These fields can be modified in
Inkscape with `CTRL-SHIFT-o` (or right click on the object and select
"Object properties"). Click on "define" after each modification.

Here is the dictionary

"ocaml id" means a string that can be used for naming an ocaml
variable: starts with a lowercase letter (a-z), and the only allowed
char besides (a-z), (A-Z), and (0-9) is underscore '_'.  See
[Ocaml Manual](https://v2.ocaml.org/manual/lex.html#sss:lex:identifiers).
It is safe to leave Inkscape's default ID.

Each Bogue widget will be inserted into a layout for positioning. The
layout name is the same as the widget name with `"_l"` appended.



| Inkscape object    | Inkscape ID           | Inkscape Label          | Inkscape Title | Inkscape Description | Bogue type                  | Bogue identifier | Bogue name          | Bogue parameters                       |
|--------------------|-----------------------|-------------------------|----------------|----------------------|-----------------------------|------------------|---------------------|----------------------------------------|
| Rectangle (R)      | [any unique ocaml id] | #Box or #rectxxx        | [any string]   |                      | Box widget                  | Inkscape ID      | Inkscape Title TODO |                                        |
| Rectangle (R)      | [any unique ocaml id] | #Button                 | [any string]   |                      | Button widget               | Inkscape ID      | Inkscape Title TODO | label = Inkscape Desc                  |
| Linked Image (C-I) | [any unique ocaml id] | #Image or #imagexxx     | [any string]   |                      | Image widget                | Inkscape ID      | Inkscape Title TODO |                                        |
| Linked Image (C-I) | [any unique ocaml id] | #Button                 | [any string]   |                      | Button widget TODO          | Inkscape ID      | Inkscape Title TODO |                                        |
| Group (C-S-O)      | [any unique ocaml id] | #Layout or #gxxx        | [any string]   |                      | Layout                      | Inkscape ID      | Inkscape Title      |                                        |
| Group (C-S-O)      | [any unique ocaml id] | #Button                 | [any string]   |                      | Button widget w/ label TODO | Inkscape ID      | Inkscape Title      | group should contain a rect and a text |
| Connector (O)      | [any unique ocaml id] | #Connection or #pathxxx | [any ocaml id] |                      | Connection                  | Inkscape ID      | N/A                 | action = Inkscape Title                |
|------|-------|--------|-----|-------|---------|-------|------|----------------|
| Layer              | layerxxx              |                         |                |                      | layer                       |                  |                     |                                        |



Special setting:


--------------------------------------------|-------------
 Document properties --> Metadata --> Title | Bogue title 


## Notes

+ When setting an object parameter in Inkscape, don't forget to click
  on "Define" to validate your entry.
+ [any unique ocaml id] shoud be a valid Ocaml identifier; it will be
  sanitized if not the case.
+ When *importing an image* into the Inkscape document, you should
  select *"Link"* and not "Embed".

## Inkscape hints

+ when creating the document, select a "real size" that fits your
  screen/GUI. For instance 80mm x 50mm for a small window.  Then,
  adjust the Inkscape zoom to obtain the theoretical size (check with
  a ruler! 100% is not always good! On my screen I need to use 125%).

+ It is advisable to work with a grid and align all objects as much as
  possible

+ When creating a text element, just click and type: don't set up a
  text zone apriori.


## Examples

TODO

## TODO

TODO
