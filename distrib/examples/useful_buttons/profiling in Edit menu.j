/* $Id$ */

MENU Edit IS
  SEPARATOR
  CHECKBOX profiling "Collect execution profile" INITIALLY false
  BUTTON "Reset execution profile"  IS profile reset
  BUTTON "Save profile report"      IS profile report
END
