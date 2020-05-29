package ui;

import compilation.Pipeline;
import javafx.application.Platform;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import org.fxmisc.richtext.CodeArea;
import org.fxmisc.richtext.LineNumberFactory;
import org.fxmisc.richtext.model.StyleSpans;
import org.fxmisc.richtext.model.StyleSpansBuilder;
import org.fxmisc.wellbehaved.event.EventPattern;
import org.fxmisc.wellbehaved.event.InputMap;
import org.fxmisc.wellbehaved.event.Nodes;
import source.SourceOutput;

import java.util.Collection;
import java.util.Collections;
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

        Pipeline pipeline = new Pipeline();
        for (String line : lines)
            pipeline.compile(line);

        SourceOutput sourceOutput = pipeline.compile(input);
        pipeline.printParseTree();

        return sourceOutput;
    }

    private void initialiseCodeArea()
    {
        /* Sourced from https://github.com/FXMisc/RichTextFX */
        codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea));
        codeArea.multiPlainChanges()
                .subscribe(ignore -> codeArea.setStyleSpans(0, computeHighlighting(codeArea.getText().toLowerCase())));

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

    private static final String[] KEYWORDS = new String[]{"if", "else", "loop", "from", "to", "and", "or"};
    private static final String[] OPERATORS = new String[]{"\\+", "\\-", "\\*", "\\/", "\\%", "\\^", "\\>", "\\<", "\\=", "\\!"};
    private static final String[] BOOLEANS = new String[]{"true", "false"};

    private static final String KEYWORD_PATTERN = "\\b(" + String.join("|", KEYWORDS) + ")\\b";
    private static final String BOOLEAN_PATTERN = "\\b(" + String.join("|", BOOLEANS) + ")\\b";
    private static final String OPERATOR_PATTERN = "(" + String.join("|", OPERATORS) + ")";
    private static final String PAREN_PATTERN = "\\(|\\)";
    private static final String BRACE_PATTERN = "\\{|\\}";
    private static final String STRING_PATTERN = "\"([^\"\\\\]|\\\\.)*\"";
    private static final String COMMENT_PATTERN = "#[^\n]*" + "|" + "/\\*(.|\\R)*?\\*/";

    private static final Pattern PATTERN = Pattern.compile(
            "(?<KEYWORD>" + KEYWORD_PATTERN + ")"
            + "|(?<BOOLEAN>" + BOOLEAN_PATTERN + ")"
            + "|(?<OPERATOR>" + OPERATOR_PATTERN + ")"
            + "|(?<PAREN>" + PAREN_PATTERN + ")"
            + "|(?<BRACE>" + BRACE_PATTERN + ")"
            + "|(?<STRING>" + STRING_PATTERN + ")"
            + "|(?<COMMENT>" + COMMENT_PATTERN + ")"
    );

    private static StyleSpans<Collection<String>> computeHighlighting(String text)
    {
        /* Sourced from https://github.com/FXMisc/RichTextFX */
        int lastKwEnd = 0;
        Matcher matcher = PATTERN.matcher(text);
        StyleSpansBuilder<Collection<String>> spansBuilder = new StyleSpansBuilder<>();

        while (matcher.find())
        {
            String styleClass =
                    matcher.group("KEYWORD") != null ? "keyword" :
                    matcher.group("BOOLEAN") != null ? "boolean" :
                    matcher.group("OPERATOR") != null ? "operator" :
                    matcher.group("PAREN") != null ? "paren" :
                    matcher.group("BRACE") != null ? "brace" :
                    matcher.group("STRING") != null ? "string" :
                    matcher.group("COMMENT") != null ? "comment" :
                    null;
            assert styleClass != null;

            spansBuilder.add(Collections.emptyList(), matcher.start() - lastKwEnd);
            spansBuilder.add(Collections.singleton(styleClass), matcher.end() - matcher.start());
            lastKwEnd = matcher.end();
        }
        spansBuilder.add(Collections.emptyList(), text.length() - lastKwEnd);

        return spansBuilder.create();
    }
}
