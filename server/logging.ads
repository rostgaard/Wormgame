package Logging is

   type Logging_Level is (Critical, Important, Message);

   procedure Log (Level       : in Logging_Level;
                  The_Message : in String);

   protected Something

end Logging;
