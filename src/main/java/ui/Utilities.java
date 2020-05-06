package ui;

import javafx.stage.Screen;

public final class Utilities
{
    public static double getWindowWidth()
    {
        return Screen.getPrimary().getBounds().getWidth() * 0.4;
    }

    public static double getWindowHeight()
    {
        return Screen.getPrimary().getBounds().getHeight() * 0.75;
    }


}
