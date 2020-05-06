package ui;

import javafx.scene.layout.Background;
import javafx.scene.layout.VBox;
import javafx.scene.text.TextFlow;
import lombok.Getter;

@Getter
public final class OutputBox
{
    private final TextFlow textFlow;
    private final VBox box;

    public OutputBox()
    {
        this.textFlow = new TextFlow();
        this.box = generateOutputBox(this.textFlow);
    }

    private static VBox generateOutputBox(TextFlow textFlow)
    {
        VBox box = new VBox(textFlow);
        box.setBackground(Background.EMPTY);
        String style = "-fx-background-color: rgba(0, 0, 0, 0.9);";
        box.setStyle(style);

        return box;
    }
}
