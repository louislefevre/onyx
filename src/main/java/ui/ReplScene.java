package ui;

import compilation.Pipeline;
import javafx.geometry.Orientation;
import javafx.scene.Scene;
import javafx.scene.control.Accordion;
import javafx.scene.control.Label;
import javafx.scene.control.Separator;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import source.SourceOutput;

import java.util.LinkedList;


public final class ReplScene
{
    private static final String TITLE = "Onyx REPL";
    private static final int LINE_LIMIT = 20;

    public void setStage(Stage stage)
    {
        stage.setTitle(TITLE);
        stage.setWidth(Utilities.getWindowWidth()*0.5);
        stage.setHeight(Utilities.getWindowHeight()*0.5);
        stage.setScene(createScene());
        stage.show();
    }

    private Scene createScene()
    {
        Pipeline pipeline = new Pipeline();
        pipeline.enableReplMode();

        TitledPane panel = new TitledPane("Symbols" , new Label("Show all declared symbols"));
        Accordion accordion = new Accordion(panel);
        TextField inputText = new TextField();
        Text outputText = new Text();
        Separator separator = new Separator(Orientation.HORIZONTAL);
        Text historyText = new Text();
        VBox box = new VBox(accordion, inputText, outputText, separator, historyText);

        LinkedList<String> lines = new LinkedList<>();

        inputText.setOnKeyPressed(key -> {
            if (key.getCode().equals(KeyCode.ENTER))
            {
                String input = inputText.getText();
                if(input.isBlank())
                    return;

                SourceOutput sourceOutput = pipeline.compile(input);
                String output = sourceOutput.getSimpleResult().toString();

                outputText.setText(output);

                if(lines.size() >= LINE_LIMIT)
                    lines.removeLast();

                StringBuilder historyBuilder = new StringBuilder();
                lines.forEach(historyBuilder::append);
                historyText.setText(historyBuilder.toString());

                String text = output + System.getProperty("line.separator");
                lines.addFirst(text);

                StringBuilder symbolBuilder = new StringBuilder();
                pipeline.getSymbolTable().getSymbols().forEach((k,v) -> {
                    String symbol = String.format("%1s = %2s (%3s)", k, v.getValue(), v.getType());
                    symbolBuilder.append(symbol);
                    symbolBuilder.append(System.getProperty("line.separator"));
                });

                panel.setContent(new Label(symbolBuilder.toString()));

                inputText.clear();
            }
        });

        return new Scene(box);
    }
}
