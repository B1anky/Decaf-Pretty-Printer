import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class Jpp extends DecafBaseListener {


  public static void main(String[] args) throws Exception {
    ANTLRInputStream input = new ANTLRInputStream(System.in);
    DecafLexer lexer = new DecafLexer(input);
    lexer.removeErrorListeners();
    CountingErrorListener errors = CountingErrorListener.INSTANCE;
    lexer.addErrorListener(errors);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    DecafParser parser = new DecafParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(errors);

    /* if using a listener
    ParseTree tree = parser.start();
    if no errors {
      Jpp listener = new Jpp(tokens); //Jpp must extend DecafBaseListener
      ParseTreeWalker walker = new ParseTreeWalker();
      walker.walk(listener, tree);
      //code to print out text
    }
    */

    /* if using a visitor
    Jpp visitor = new Jpp(tokens); //Jpp must extend DecafBaseVisitor
    parser.start().accept(visitor);
    if no errors {
      //code to print out text
    }
    */

    /* if using an ast
    Ast ast = parser.start();
    if no errors {
      //code to print out ast text
    }
    */

  }

}
