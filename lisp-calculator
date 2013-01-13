import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.List;
import java.util.ArrayList;
public class LispCalculator{
    public static final Pattern operateP = Pattern.compile("\\s*([^\\s\\(\\)]+)");
    public static final Pattern numP = Pattern.compile("\\s+([\\+\\-]?(?:(?:[1-9]\\d*)|(?:\\.\\d+)|(?:\\d+\\.\\d+)))");
    public static final Pattern bracketP = Pattern.compile("\\s*(\\(|\\))");

    public static final List<Pattern> tokenPs = new ArrayList<Pattern>();

    /** decimal match pattern **/
    public static final Pattern decimalP = Pattern.compile("[\\+\\-]?(?:(?:\\.\\d+)|(?:\\d+\\.\\d+))");

    /** integer match pattern **/
    public static final Pattern integerP = Pattern.compile("[\\+\\-]?[1-9]\\d*");

    /** if output debug info **/
    public static boolean debug = false;

    static{
        tokenPs.add(operateP);
        tokenPs.add(numP);
        tokenPs.add(bracketP);
    }
    
    private String token;
    private String exp;
    private String origExp;
    private boolean decimalRet = false;
    
    private void readToken(){
        token = null;
        for(Pattern p:tokenPs){
            Matcher m = p.matcher(exp);
            if(m.lookingAt()){
                token = m.group(1);
                int end = m.end();
                exp = exp.substring(end);
                break;
            }
        }

    }

    public double eval(String exp){
        this.exp = exp;
        this.origExp = new String(exp);
        double ret = -1;
        readToken();
        while(token != null){
            ret = exp1();
        }

        return ret;
    }

    private boolean peek(String str){
        return token == null || str.equals(token);
    }
    private boolean accept(String str){
        if(str.equals(token)){
            readToken();
            return true;
        }
        return false;
    }

    private boolean expect(String str){
        if(accept(str) == false){
            if(token != null)
                System.err.printf("unexpected token %s, expect \"%s\"\n",token,str);
            else
                System.err.printf("has no more token, expect \"%s\"\n",str);
            System.exit(1);
        }
        return true;
    }

    private void debug(String info){
        if(debug){
            System.out.println(info);
        }
    }

    private void debug(String templ, Object... args){
        if(debug){
            System.out.printf(templ, args);
        }
    }


    private boolean isInteger(String token){
        return integerP.matcher(token).matches();
    }

    private String delPlus(String token){
        if(token.startsWith("+")){
            token = token.substring(1);
        }

        return token;
    }

    private int parseInteger(String token){
        token = delPlus(token);
        return Integer.parseInt(token);
    }

    private boolean isDecimal(String token){
        return decimalP.matcher(token).matches();
    }

    private double parseDecimal(String token){
        token = delPlus(token);
        return Double.parseDouble(token);
    }

    public static final Pattern numRegExp = Pattern.compile("[\\+\\-]?(?:(?:[1-9]\\d*)|(?:\\.\\d+)|(?:\\d+\\.\\d+))");

    private double exp1(){
        double ret = 0;
        if(numRegExp.matcher(token).matches()){
            if(isInteger(token)){
                ret = parseInteger(token);
            }else if(isDecimal(token)){
                decimalRet = true;
                ret = parseDecimal(token);
            }
            readToken();
        }else if(accept("(")){
            ret = exp2();
            expect(")");
        }else{
            System.err.printf("Unexpected token \"%s\"\n",token);
            System.exit(1);
        }
        return ret;
    }

    private double exp2(){
        double ret = 0;
        if(accept("*")){
            ret = 1;
            while(peek(")") == false){
                ret = ret * exp1();;
            }
        }else if(accept("/")){
            ret = exp1();
            if(peek(")") == false){
                while(peek(")") == false){
                  ret = ret / exp1();;
                }
            }else{
                ret = 1/ ret;
            }
        }else if(accept("+")){
            ret = 0;
            while(peek(")") == false){
                ret = ret + exp1();;
            }
        }else if(accept("-")){
            ret = 0;
            while(peek(")") == false){
                ret = ret - exp1();;
            }
        }else{
            System.err.printf("operator token expected ,actual token %s.\n",token);
            System.exit(1);
        }
        return ret;
    }


    public static void main(String[] args){
        if(args.length == 0){
            System.out.println("Usage : java LispCalculator (+ 3 22)");
            System.exit(1);
        }

        debug = true;

        LispCalculator cal = new LispCalculator();
        System.out.println(cal.eval(args[0]));
    }
}
