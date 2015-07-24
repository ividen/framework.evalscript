import com.ividen.evalscript.*;
import scala.math.BigDecimal;


/**
 * Created by alexander.guzanov on 7/23/15.
 */
public class TestClass extends CompiledScript {
    static final Literal v1 = new DecimalLiteral(BigDecimal.valueOf(10));
    static final Literal v2 = new DecimalLiteral(BigDecimal.valueOf(20));

    public TestClass(GlobalContext globals) {
        super(globals);
    }
    @Override
    public void execute() {
        setGlobal("v",v1);
        if(checkCondition(getGlobal("v"))) setGlobal("v",v2);



//

    }

}
