package ui;

import javafx.scene.control.TextArea;
import javafx.scene.layout.VBox;
import lombok.Getter;

@Getter
public final class InputBox
{
    private final TextArea textArea;
    private final VBox box;

    public InputBox()
    {
        this.textArea = createTextArea();
        this.box = generateInputBox(this.textArea);
    }

    private static TextArea createTextArea()
    {
        TextArea textArea = new TextArea();
        textArea.setMinHeight(PrimaryInterface.getWindowHeight() * 0.8);
        textArea.setWrapText(true);
        return textArea;
    }

    private static VBox generateInputBox(TextArea textArea)
    {
        return new VBox(textArea);
    }
}
