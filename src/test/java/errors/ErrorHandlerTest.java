package errors;

import org.junit.jupiter.api.Test;
import source.SourceInput;
import source.SourceOutput;
import util.TestFactory;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ErrorHandlerTest
{
    @Test
    public void errorHandlerCatchesLexerErrors()
    {
        String message = "Failed to catch lexical error: ";
        HashMap<String, Error> lexicalErrorCollection = TestFactory.lexicalErrorCollection();

        lexicalErrorCollection.forEach((input, error) -> {
            String actual = compile(input);
            String expected = getErrorMessage(input, error);
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void errorHandlerCatchesSyntaxErrors()
    {
        String message = "Failed to catch syntax error: ";
        HashMap<String, Error> syntaxErrorCollection = TestFactory.syntaxErrorCollection();

        syntaxErrorCollection.forEach((input, error) -> {
            String actual = compile(input);
            String expected = getErrorMessage(input, error);
            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void errorHandlerCatchesSemanticErrors()
    {
        String message = "Failed to catch semantic error: ";
        HashMap<String, Error> semanticErrorCollection = TestFactory.semanticErrorCollection();

        semanticErrorCollection.forEach((input, error) -> {
            String actual = compile(input);
            String expected = getErrorMessage(input, error);
            assertEquals(expected, actual, message + input);
        });
    }

    private String compile(String input)
    {
        SourceOutput sourceOutput = TestFactory.createCompiler().compileInput(input);
        return sourceOutput.getRawOutput();
    }

    private String getErrorMessage(String input, Error error)
    {
        SourceInput sourceInput = new SourceInput(input);
        return error.getErrorMessage(sourceInput).toString();
    }
}