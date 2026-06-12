package io.github.edadma.typesetter

/** A control item that, issued inside a paragraph, belongs to the enclosing vertical list rather than to any one
  * line: line building lifts it out of its line and re-adds it to the vertical list right after that line, where
  * the page builder can see it. Marks and inserts migrate; penalties do not — and a migrated item is placed before
  * any interline penalty, because a non-discardable item between a penalty and the following glue would make that
  * glue a legal breakpoint again.
  */
trait MigratingBox extends ControlBox
