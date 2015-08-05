import com.ividen.evalscript.CompiledScript;
import com.ividen.evalscript.DecimalLiteral;
import com.ividen.evalscript.Literal;
import scala.collection.JavaConversions$;
import scala.collection.immutable.Map;
import scala.collection.immutable.Map$;
import scala.math.BigDecimal;

import java.util.HashMap;


/**
 * Created by alexander.guzanov on 7/23/15.
 */
public class TestClass extends CompiledScript {
    static final Literal v1 = new DecimalLiteral(BigDecimal.valueOf(10));
    static final Literal v2 = new DecimalLiteral(BigDecimal.valueOf(20));
    static final Literal v3 = new DecimalLiteral(BigDecimal.valueOf(30));
    private Literal g_result;

    public TestClass(Map<String, Literal> globals) {
        super(globals);

    }

    @Override
    public void execute() {
        Literal l = v1;

        g_result = v1;

    }


    @Override
    public Map<String, Literal> getGlobals() {
        java.util.Map<String, Literal> result = new HashMap<>();
        result.put("multiplier", g_result);
        return Map$.MODULE$.apply(JavaConversions$.MODULE$.asScalaMap(result).toSeq());
    }
}
