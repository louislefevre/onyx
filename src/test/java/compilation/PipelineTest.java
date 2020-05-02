package compilation;

import org.junit.jupiter.api.Test;
import utilities.TestHub;

import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PipelineTest
{
    @Test
    public void pipelineCompilesAssignmentOperators()
    {
        String message = "Failed to compile variable assignment operator: ";
        HashMap<String[], Object> assignmentOperatorsCollection = TestHub.assignmentOperatorsCollection();

        assignmentOperatorsCollection.forEach((inputArray, expected) -> {
            Pipeline pipeline = createPipeline();
            String input = null;
            Object actual = null;

            for (String text : inputArray)
            {
                input = text;
                actual = pipeline.compile(text).getResult();
            }

            assertEquals(expected, actual, message + input);
        });
    }

    @Test
    public void pipelineCompilesReassignment()
    {
        String message = "Failed to compile variable reassignment: ";
        HashMap<String[], Object> reassignmentCollectionCollection = TestHub.reassignmentCollection();

        reassignmentCollectionCollection.forEach((inputArray, expected) -> {
            Pipeline pipeline = createPipeline();
            String input = null;
            Object actual = null;

            for (String text : inputArray)
            {
                input = text;
                actual = pipeline.compile(text).getResult();
            }

            assertEquals(expected, actual, message + input);
        });
    }

    private Pipeline createPipeline()
    {
        return TestHub.createPipeline();
    }
}
