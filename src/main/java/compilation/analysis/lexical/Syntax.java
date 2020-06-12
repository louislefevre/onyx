package compilation.analysis.lexical;

/**
 * The Syntax class is used to hold constant String variables that identify valid Onyx syntax.
 * <p>
 * This class is solely used for the purpose of storing recognised Onyx syntax as constants. Other classes which
 * need to refer to the syntax for each keyword or symbol may do so here, as it ensures all code remains
 * consistent by using variables from the same location.
 *
 * @author Louis Lefevre
 * @version 1.0
 * @since 1.0
 */
public final class Syntax
{
    // Data Type Characters
    public static final String DECIMAL_POINT_SYNTAX = ".";
    public static final String DOUBLE_QUOTES_SYNTAX = "\"";

    // Reserved Words
    public static final String TRUE_SYNTAX = "true";
    public static final String FALSE_SYNTAX = "false";
    public static final String IF_SYNTAX = "if";
    public static final String ELSE_SYNTAX = "else";
    public static final String LOOP_SYNTAX = "loop";
    public static final String FROM_SYNTAX = "from";
    public static final String TO_SYNTAX = "to";
    public static final String AND_SYNTAX = "and";
    public static final String OR_SYNTAX = "or";

    // Separators
    public static final String OPEN_BRACE_SYNTAX = "{";
    public static final String CLOSE_BRACE_SYNTAX = "}";
    public static final String OPEN_PARENTHESIS_SYNTAX = "(";
    public static final String CLOSE_PARENTHESIS_SYNTAX = ")";

    // Unary Operators
    public static final String NOT_SYNTAX = "!";

    // Mathematical Binary Operators
    public static final String PLUS_SYNTAX = "+";
    public static final String MINUS_SYNTAX = "-";
    public static final String STAR_SYNTAX = "*";
    public static final String SLASH_SYNTAX = "/";
    public static final String PERCENT_SYNTAX = "%";
    public static final String CARET_SYNTAX = "^";

    // Conditional Binary Operators
    public static final String GREATER_SYNTAX = ">";
    public static final String GREATER_EQUALS_SYNTAX = ">=";
    public static final String LESS_SYNTAX = "<";
    public static final String LESS_EQUALS_SYNTAX = "<=";
    public static final String EQUALS_EQUALS_SYNTAX = "==";

    // Assignment Operators
    public static final String EQUALS_SYNTAX = "=";
    public static final String NOT_EQUALS_SYNTAX = "!=";
    public static final String PLUS_EQUALS_SYNTAX = "+=";
    public static final String MINUS_EQUALS_SYNTAX = "-=";
    public static final String STAR_EQUALS_SYNTAX = "*=";
    public static final String SLASH_EQUALS_SYNTAX = "/=";
    public static final String PERCENT_EQUALS_SYNTAX = "%=";
    public static final String CARET_EQUALS_SYNTAX = "^=";

    // Break Syntax
    public static final String LINE_BREAK_SYNTAX = System.lineSeparator();
    public static final String EOF_SYNTAX = "\0";

    // Not-parsed Syntax
    public static final String HASH_SYNTAX = "#";
}
