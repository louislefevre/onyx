package ui;

import javafx.scene.control.TextArea;
import javafx.scene.layout.VBox;
import lombok.Getter;

@Getter
public final class InputBox
{
    private final VBox box;

    public InputBox()
    {
        this.box = generateInputBox();
    }

    private static VBox generateInputBox()
    {
        return new VBox(createTextArea());
    }

    private static TextArea createTextArea()
    {
        TextArea textArea = new TextArea();
        textArea.setMinHeight(PrimaryInterface.getWindowHeight()*0.8);
        textArea.setWrapText(true);
        return textArea;
    }
}
