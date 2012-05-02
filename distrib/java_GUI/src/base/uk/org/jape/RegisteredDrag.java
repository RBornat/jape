package uk.org.jape;

import java.awt.event.MouseEvent;

// because Mac OS X can lose a drag if you touch the mouse wheel, it's necessary to register drags with the 
// canvas and be able to release them.
public abstract class RegisteredDrag {
    public abstract void released(MouseEvent e);
}
