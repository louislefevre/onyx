package errors;

import org.junit.jupiter.api.Test;
import source.SourceOutput;
import utilities.TestHub;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertTrue;

class ErrorHandlerTest
{
    @Test
    public void errorHandlerCatchesLexerErrors()
    {
        String message = "Failed to catch lexical error: ";
        HashMap<String, String> lexicalErrorCollection = TestHub.lexicalErrorCollection();

        lexicalErrorCollection.forEach((input, expected) -> {
            boolean actual = containsError(input, expected);
            assertTrue(actual, message + input);
        });
    }

    @Test
    public void errorHandlerCatchesSyntaxErrors()
    {
        String message = "Failed to catch syntax error: ";
        HashMap<String, String> syntaxErrorCollection = TestHub.syntaxErrorCollection();

        syntaxErrorCollection.forEach((input, expected) -> {
            boolean actual = containsError(input, expected);
            assertTrue(actual, message + input);
        });
    }

    @Test
    public void errorHandlerCatchesSemanticErrors()
    {
        String message = "Failed to catch semantic error: ";
        HashMap<String, String> semanticErrorCollection = TestHub.semanticErrorCollection();

        semanticErrorCollection.forEach((input, expected) -> {
            boolean actual = containsError(input, expected);
            assertTrue(actual, message + input);
        });
    }

    private boolean containsError(String input, String expected)
    {
        SourceOutput sourceOutput = TestHub.createSourceOutput(input);
        String actual = sourceOutput.getDecoratedOutput().toString();
        return actual.contains(expected);
    }
}