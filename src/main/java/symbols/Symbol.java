package symbols;

import identifiers.ObjectType;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
public final class Symbol
{
    private final String name;
    @Setter private Object value;
    @Setter private ObjectType type;
    @Setter private List<Object> valueHistory;

    public Symbol(String name, Object value, ObjectType type)
    {
        this.name = name;
        this.value = value;
        this.type = type;
        this.valueHistory = new ArrayList<>();
    }

    public void addHistoryValue(Object value)
    {
        this.valueHistory.add(value);
    }
}
