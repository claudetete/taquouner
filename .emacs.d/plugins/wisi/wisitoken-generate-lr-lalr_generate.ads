--  Abstract :
--
--  Generalized LALR parse table generator.
--
--  Copyright (C) 2002 - 2003, 2009 - 2010, 2013 - 2015, 2017, 2018 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Generate.LR1_Items;
with WisiToken.Productions;
package WisiToken.Generate.LR.LALR_Generate is

   function Generate
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Known_Conflicts : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      McKenzie_Param  : in McKenzie_Param_Type := Default_McKenzie_Param;
      Put_Parse_Table : in Boolean := False)
     return Parse_Table_Ptr
   with Pre =>
     Descriptor.Last_Lookahead = Descriptor.First_Nonterminal and
     Descriptor.First_Nonterminal = Descriptor.Accept_ID;
   --  Generate a generalized LALR parse table for Grammar. The
   --  grammar start symbol is the LHS of the first production in
   --  Grammar.
   --
   --  Unless Ignore_Unused_Tokens is True, raise Grammar_Error if
   --  there are unused tokens.
   --
   --  Unless Ignore_Unknown_Conflicts is True, raise Grammar_Error if there
   --  are unknown conflicts.

   ----------
   --  Visible for unit tests

   function LALR_Goto_Transitions
     (Kernel            : in LR1_Items.Item_Set;
      Symbol            : in Token_ID;
      First_Nonterm_Set : in Token_Array_Token_Set;
      Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor        : in WisiToken.Descriptor)
     return LR1_Items.Item_Set;
   --  Return the Item_Set that is the goto for Symbol from Kernel.
   --  If there is no such Item_Set, Result.Set is null.

   function LALR_Kernels
     (Grammar           : in WisiToken.Productions.Prod_Arrays.Vector;
      First_Nonterm_Set : in Token_Array_Token_Set;
      Descriptor        : in WisiToken.Descriptor)
     return LR1_Items.Item_Set_List;

end WisiToken.Generate.LR.LALR_Generate;
