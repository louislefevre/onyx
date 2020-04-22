package ui;

import javafx.scene.layout.Background;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import lombok.Getter;

@Getter
public final class OutputBox
{
    private final VBox box;

    public OutputBox()
    {
        this.box = generateOutputBox();
    }

    private static VBox generateOutputBox()
    {
        VBox box = new VBox(createText());
        box.setBackground(Background.EMPTY);
        String style = "-fx-background-color: rgba(0, 0, 0, 0.9);";
        box.setStyle(style);

        return box;
    }

    private static Text createText()
    {
        Text text = new Text();
        text.setFill(Color.RED);
        text.setFont(Font.font("Monospaced Regular"));
        text.setWrappingWidth(PrimaryInterface.getWindowWidth());
        text.setText("This is the text to displayThis is the text to d" +
                     "isplayThis is the text to displayThis is the text to dis" +
                     "play displayThis is the text to display");
        return text;
    }
}
