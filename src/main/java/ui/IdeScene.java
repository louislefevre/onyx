package ui;

import javafx.application.Platform;
import javafx.geometry.Orientation;
import javafx.scene.Scene;
import javafx.scene.control.MenuBar;
import javafx.scene.control.SplitPane;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Background;
import javafx.scene.layout.BorderPane;
import javafx.scene.text.TextFlow;
import javafx.stage.Stage;
import lombok.Getter;
import org.fxmisc.flowless.VirtualizedScrollPane;
import org.fxmisc.richtext.CodeArea;
import org.fxmisc.richtext.LineNumberFactory;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Getter
public final class IdeScene
{
    private static final String TITLE = "Onyx Compiler";
    private final CodeArea inputArea;
    private final TextFlow outputArea;

    public IdeScene()
    {
        this.inputArea = createCodeArea();
        this.outputArea = createOutputArea();
    }

    public void setStage(Stage stage)
    {
        stage.setTitle(TITLE);
        stage.setWidth(Utilities.getWindowWidth());
        stage.setHeight(Utilities.getWindowHeight());
        stage.setScene(createScene());
        stage.show();
    }

    private Scene createScene()
    {
        MenuBox menuBox = new MenuBox(this);
        MenuBar menu = menuBox.getMenuBar();

        VirtualizedScrollPane inputPane = new VirtualizedScrollPane<>(inputArea);

        inputPane.setPrefHeight(Utilities.getWindowHeight() * 0.7);
        outputArea.setPrefHeight(Utilities.getWindowHeight() * 0.3);

        SplitPane splitPane = new SplitPane(inputPane, outputArea);
        splitPane.setDividerPosition(0, 0.7);
        splitPane.setOrientation(Orientation.VERTICAL);

        BorderPane borderPane = new BorderPane();
        borderPane.setTop(menu);
        borderPane.setCenter(splitPane);

        return new Scene(borderPane);
    }

    public static CodeArea createCodeArea()
    {
        CodeArea codeArea = new CodeArea();
        codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea));

        final Pattern whiteSpace = Pattern.compile("^\\s+");
        codeArea.addEventHandler(KeyEvent.KEY_PRESSED, KE ->
        {
            if (KE.getCode() == KeyCode.ENTER)
            {
                int caretPosition = codeArea.getCaretPosition();
                int currentParagraph = codeArea.getCurrentParagraph();
                Matcher m0 = whiteSpace.matcher(codeArea.getParagraph(currentParagraph - 1).getSegments().get(0));
                if (m0.find()) Platform.runLater(() -> codeArea.insertText(caretPosition, m0.group()));
            }
        });

        return codeArea;
    }

    private static TextFlow createOutputArea()
    {
        TextFlow textFlow = new TextFlow();
        textFlow.setBackground(Background.EMPTY);
        String style = "-fx-background-color: rgba(0, 0, 0, 0.9);";
        textFlow.setStyle(style);
        return textFlow;
    }
}
