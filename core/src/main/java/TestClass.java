import com.ividen.evalscript.BooleanLiteral;
import com.ividen.evalscript.DecimalLiteral;
import com.ividen.evalscript.Literal;
import com.ividen.evalscript.StringLiteral;
import scala.math.BigDecimal;


/**
 * Created by alexander.guzanov on 7/23/15.
 */
public class TestClass {
    static final Literal v1 = new DecimalLiteral(BigDecimal.valueOf(10));
    static final Literal v2 = new StringLiteral("test");
    static final Literal v3 = new BooleanLiteral(true);

}
