package ui;

import javafx.scene.layout.Background;
import javafx.scene.text.TextFlow;
import lombok.Getter;

@Getter
public final class OutputBox
{
    private final TextFlow textFlow;

    public OutputBox()
    {
        this.textFlow = createTextFlow();
    }

    private static TextFlow createTextFlow()
    {
        TextFlow textFlow = new TextFlow();
        textFlow.setBackground(Background.EMPTY);
        String style = "-fx-background-color: rgba(0, 0, 0, 0.9);";
        textFlow.setStyle(style);
        return textFlow;
    }
}
