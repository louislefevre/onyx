package compilation;

import org.junit.jupiter.api.Test;
import utilities.TestFactory;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CompilerTest
{
    @Test
    public void pipelineCompilesAssignmentOperators()
    {
        String message = "Failed to compile variable assignment operator: ";
        HashMap<String[], Object> assignmentOperatorsCollection = TestFactory.assignmentOperatorsCollection();

        assignmentOperatorsCollection.forEach((inputArray, expected) -> {
            Compiler compiler = createPipeline();
            String input = null;
            Object actual = null;

            for (String text : inputArray)
            {
                input = text;
                actual = compiler.compileInput(text).getOutput();
            }

            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void pipelineCompilesReassignment()
    {
        String message = "Failed to compile variable reassignment: ";
        HashMap<String[], Object> reassignmentCollectionCollection = TestFactory.reassignmentCollection();

        reassignmentCollectionCollection.forEach((inputArray, expected) -> {
            Compiler compiler = createPipeline();
            String input = null;
            Object actual = null;

            for (String text : inputArray)
            {
                input = text;
                actual = compiler.compileInput(text).getOutput();
            }

            assertEquals(expected, actual, message + input);
        });
    }

    private Compiler createPipeline()
    {
        return TestFactory.createPipeline();
    }
}
