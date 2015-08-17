import com.ividen.evalscript.CompiledScript;
import com.ividen.evalscript.DecimalLiteral$;
import com.ividen.evalscript.Literal;
import com.ividen.evalscript.Literal$;
import scala.collection.immutable.Map;
import scala.math.BigDecimal;

import java.util.HashMap;

/**
 * Created by alexander.guzanov on 8/13/15.
 */
public class TestClass extends CompiledScript {
    public static final Literal v1 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(1));
    public static final Literal v2 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(2));
    public static final Literal v3 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(3));
    public static final Literal v4 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(3));
    public static final Literal v5 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(3));
    public static final Literal v6 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(3));
    public static final Literal v7 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(3));
    public static final Literal v8 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(3));
    public static final Literal v9 = DecimalLiteral$.MODULE$.apply(BigDecimal.valueOf(3));
    private Literal g;
    public TestClass(Map<String,Object> globals) {
        super(globals);
    }
    @Override
    public void execute() {
        final HashMap<Object, Object> result = new HashMap<>();
        Literal$.MODULE$.resultToMap(g, "v1", result);
    }
    @Override
    public Map<String, Literal> getGlobals() {
        return null;
    }
}
