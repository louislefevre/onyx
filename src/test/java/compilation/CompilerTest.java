package compilation;

import org.junit.jupiter.api.Test;
import source.SourceOutput;
import util.TestFactory;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CompilerTest
{
    @Test
    public void pipelineCompilesAssignmentOperators()
    {
        String message = "Failed to compile variable assignment operator: ";
        HashMap<String, Object> assignmentOperatorsCollection = TestFactory.assignmentOperatorsCollection();

        assignmentOperatorsCollection.forEach((input, expected) -> {
            Object actual = compile(input);
            assertEquals(expected.toString(), actual, message + input);
        });
    }

    @Test
    public void pipelineCompilesReassignment()
    {
        String message = "Failed to compile variable reassignment: ";
        HashMap<String, Object> reassignmentCollectionCollection = TestFactory.reassignmentCollection();

        reassignmentCollectionCollection.forEach((input, expected) -> {
            Object actual = compile(input);
            assertEquals(expected.toString(), actual, message + input);
        });
    }

    private Object compile(String input)
    {
        Compiler compiler = TestFactory.createCompiler(false);
        SourceOutput output = compiler.compileInput(input);
        return output.getRawOutput();
    }
}
