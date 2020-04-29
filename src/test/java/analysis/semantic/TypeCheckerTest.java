package analysis.semantic;

import org.jetbrains.annotations.NotNull;
import utilities.TestHub;

class TypeCheckerTest
{
    @NotNull
    private static TypeChecker createTypeChecker(String input)
    {
        return TestHub.createTypeChecker(input);
    }
}