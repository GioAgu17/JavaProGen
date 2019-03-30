package prolog.checkers

object CheckerFactory {
    def getChecker(rule: String): Checker ={
      if(rule == "8.1.4.1")
        new ExtendChecker1
      else if(rule == "8.1.4.2")
        new ExtendChecker2
      else if(rule == "8.4.1")
        new SameParamsChecker1
      else if(rule == "8.4")
          new MethSignChecker1
      else if(rule == "14.17")
          new RetStmtChecker1
      else if(rule == "8.8.2")
        new ConstrSignChecker1
      else if(rule == "8.8.7")
        new ExplicitConstrInvoc1
      else
        sys.error("Checker Factory did not produce any instance of Checker")
    }
}
