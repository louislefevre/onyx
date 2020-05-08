package ui;

import compilation.Pipeline;
import javafx.application.Platform;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import org.fxmisc.richtext.CodeArea;
import org.fxmisc.richtext.LineNumberFactory;
import org.fxmisc.wellbehaved.event.EventPattern;
import org.fxmisc.wellbehaved.event.InputMap;
import org.fxmisc.wellbehaved.event.Nodes;
import source.SourceOutput;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class CodeManager
{
    private final CodeArea codeArea;
    private final TextField tabSizeField;
    private int tabSize;

    public CodeManager(CodeArea codeArea, TextField tabSizeField, int tabSize)
    {
        this.codeArea = codeArea;
        this.tabSizeField = tabSizeField;
        this.tabSize = tabSize;
        initialiseCodeArea();
    }

    public String getCodeInput()
    {
        return codeArea.getText();
    }

    public String getLineInfo()
    {
        int lineNum = codeArea.getCurrentParagraph() + 1;
        int columnNum = codeArea.getCaretColumn() + 1;
        return String.format("%s:%s", lineNum, columnNum);
    }

    public void refreshTabSize()
    {
        String text = tabSizeField.getText();
        int size;

        try
        {
            size = Integer.parseInt(text);
            if (size < 1 || size > 99)
                size = tabSize;
        }
        catch (NumberFormatException exception)
        {
            size = tabSize;
            System.out.println(exception.getMessage());
        }

        tabSize = size;
        String sizeText = String.valueOf(size);
        tabSizeField.setText(sizeText);
        tabSizeField.positionCaret(sizeText.length());
    }

    public void limitTabSize()
    {
        int limit = 2;
        if (tabSizeField.getText().length() > limit)
        {
            String s = tabSizeField.getText().substring(0, limit);
            tabSizeField.setText(s);
            tabSizeField.positionCaret(limit);
        }
    }

    public SourceOutput readInput(String input)
    {
        input += System.getProperty("line.separator"); // Adds extra line separator at end to avoid collision with EOF
        input = input.replaceAll("\011", ""); // Ignores horizontal tabs, breaks line separators otherwise
        String[] lines = input.split(System.getProperty("line.separator")); // Splits each line up to be run individually

        List<String> linesList = new ArrayList<>();
        for (String line : lines)
            if (!line.isBlank()) // Only adds non-blank lines
                linesList.add(line);

        Pipeline pipeline = new Pipeline();
        for (String line : linesList)
            if (!line.isBlank())
                pipeline.compile(line);

        return pipeline.compile(input);
    }

    private void initialiseCodeArea()
    {
        codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea));

        InputMap<KeyEvent> inputMap = InputMap.consume(
                EventPattern.keyPressed(KeyCode.TAB),
                e -> codeArea.replaceSelection(" ".repeat(Math.max(0, tabSize)))
        );
        Nodes.addInputMap(codeArea, inputMap);

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
    }
}
