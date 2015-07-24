import com.ividen.evalscript.*;
import scala.math.BigDecimal;


/**
 * Created by alexander.guzanov on 7/23/15.
 */
public class TestClass extends CompiledScript {
    static final Literal v1 = new DecimalLiteral(BigDecimal.valueOf(10));
    static final Literal v2 = new DecimalLiteral(BigDecimal.valueOf(20));

    public Literal multiplier;

    public TestClass(GlobalContext globals) {
        super(globals);
        multiplier = NullLiteral$.MODULE$;
    }
    @Override
    public void execute() {

    }

}
