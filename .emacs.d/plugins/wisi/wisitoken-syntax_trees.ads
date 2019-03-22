--  Abstract :
--
--  Syntax tree type and operations.
--
--  Rationale :
--
--  We provide Base_Tree and Tree in one package, because only Tree
--  needs an API; the only way Base_Tree is accessed is via Tree.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.

--  There is one syntax tree for each parser. There is one shared
--  Terminals array, matching the actual input text.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Finalization;
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Lexer;
package WisiToken.Syntax_Trees is

   type Base_Tree is new Ada.Finalization.Controlled with private;

   type Base_Tree_Access is access all Base_Tree;

   overriding procedure Finalize (Tree : in out Base_Tree);
   --  Free any allocated storage.

   overriding procedure Adjust (Tree : in out Base_Tree);
   --  Copy any allocated storage.

   type Tree is tagged private;

   procedure Initialize
     (Branched_Tree : in out Tree;
      Shared_Tree   : in     Base_Tree_Access;
      Flush         : in     Boolean);
   --  Set Branched_Tree to refer to Shared_Tree.

   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   Invalid_Node_Index : constant Node_Index := Node_Index'First;

   type Valid_Node_Index_Array is array (Positive_Index_Type range <>) of Valid_Node_Index;
   --  Index matches Base_Token_Array, Augmented_Token_Array

   package Valid_Node_Index_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Positive_Index_Type, Valid_Node_Index);
   --  Index matches Valid_Node_Index_Array.

   type Node_Label is (Shared_Terminal, Virtual_Terminal, Nonterm);

   type User_Data_Type is tagged limited null record;
   --  Many test languages don't need this, so we default the procedures
   --  to null.

   type User_Data_Access is access all User_Data_Type'Class;

   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access)
   is null;

   procedure Reset (User_Data : in out User_Data_Type) is null;
   --  Reset to start a new parse.

   procedure Lexer_To_Augmented
     (User_Data  : in out          User_Data_Type;
      Token      : in              Base_Token;
      Lexer      : not null access WisiToken.Lexer.Instance'Class)
     is null;
   --  Read auxiliary data from Lexer, create an Augmented_Token, store
   --  it in User_Data. Called before parsing, once for each token in the
   --  input stream.

   procedure Delete_Token
     (Data        : in out User_Data_Type;
      Token_Index : in     WisiToken.Token_Index)
   is null;
   --  Token at Token_Index was deleted in error recovery; update
   --  remaining tokens as needed. Called from Execute_Actions for each
   --  deleted token, before processing the syntax tree.

   procedure Reduce
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Nonterm   : in     Valid_Node_Index;
      Tokens    : in     Valid_Node_Index_Array)
   is null;
   --  Reduce Tokens to Nonterm. Nonterm.Byte_Region is computed by
   --  caller.

   type Semantic_Action is access procedure
     (User_Data : in out User_Data_Type'Class;
      Tree      : in out Syntax_Trees.Tree;
      Nonterm   : in     Valid_Node_Index;
      Tokens    : in     Valid_Node_Index_Array);
   --  Routines of this type are called by
   --  WisiToken.LR.Parser.Execute_Actions when it processes a Nonterm
   --  node in the syntax tree. Tokens are the children of Nonterm.

   Null_Action : constant Semantic_Action := null;

   procedure Clear (Tree : in out Syntax_Trees.Base_Tree);
   procedure Clear (Tree : in out Syntax_Trees.Tree);
   --  Delete all Elements and free associated memory; keep results of
   --  Initialize.

   procedure Flush (Tree : in out Syntax_Trees.Tree);
   --  Move all nodes in branched part to shared tree, set Flush mode
   --  True.

   procedure Set_Flush_False (Tree : in out Syntax_Trees.Tree);
   --  Set Flush mode False; use Flush to set True.

   function Flushed (Tree : in Syntax_Trees.Tree) return Boolean;

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     Production_ID;
      Children        : in     Valid_Node_Index_Array;
      Action          : in     Semantic_Action;
      Default_Virtual : in     Boolean)
     return Valid_Node_Index
   with
     Pre  => not Tree.Traversing,
     Post => Tree.Is_Empty (Add_Nonterm'Result) or
             Tree.Min_Terminal_Index (Add_Nonterm'Result) /= Invalid_Token_Index;
   --  Add a new Nonterm node, which can be empty. Result points to the
   --  added node. If Children'Length = 0, set Nonterm.Virtual :=
   --  Default_Virtual.

   function Add_Terminal
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Token_Index;
      Terminals : in     Base_Token_Arrays.Vector)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Terminal node. Terminal must be an index into Terminals.
   --  Result points to the added node.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new virtual terminal node with no parent. Result points to
   --  the added node.

   procedure Set_State
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Index;
      State : in     State_Index);

   function State (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Unknown_State_Index;

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Label;

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Branched_Nodes (Tree : in Syntax_Trees.Tree) return Boolean;
   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Valid_Node_Index_Array) return Boolean;
   function Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean;

   function Parent (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index;

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   with Pre => Tree.Is_Nonterm (Node);

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID;

   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Buffer_Region;

   function Same_Token
     (Tree_1  : in Syntax_Trees.Tree'Class;
      Index_1 : in Valid_Node_Index;
      Tree_2  : in Syntax_Trees.Tree'Class;
      Index_2 : in Valid_Node_Index)
     return Boolean;
   --  True if the two tokens have the same ID and Byte_Region.

   function Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Recover_Token;

   function Recover_Token_Array
     (Tree  : in Syntax_Trees.Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Recover_Token_Array;
   --  For non-virtual terminals, copied from Tree.Terminals. For others,
   --  constructed from Tree data.

   procedure Set_Augmented
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Index;
      Value : in     Base_Token_Class_Access)
   with Pre => Tree.Is_Nonterm (Node);
   --  Value will be deallocated when Tree is finalized.

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Base_Token_Class_Access
   with Pre => Tree.Is_Nonterm (Node);
   --  Returns result of Set_Augmented.

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   with Pre => Tree.Is_Nonterm (Node);

   function Find_Ancestor
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index;
   --  Return the ancestor of Node that contains ID, or Invalid_Node_Index if
   --  none match.

   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Has_Parent (Node);
   --  Return the sibling of Node that contains ID, or Invalid_Node_Index if
   --  none match.

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Is_Nonterm (Node);
   --  Return the child of Node that contains ID, or Invalid_Node_Index if
   --  none match.

   function Find_Descendant
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index;
   --  Return the child of Node that contains ID (may be Node), or
   --  Invalid_Node_Index if none match.

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; Root : in Valid_Node_Index);

   function Root (Tree : in Syntax_Trees.Tree) return Node_Index;
   --  Return value set by Set_Root; defaults to the last node added.
   --  returns Invalid_Node_Index if Tree is empty.

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index));
   --  Traverse Tree in depth-first order, calling Process_Node on each
   --  node, starting at Tree.Root.

   function Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index
   with Pre => Tree.Is_Terminal (Node);

   function Min_Terminal_Index (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index;
   function Max_Terminal_Index (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index;
   --  Returns lowest/highest index of shared terminal in subtree under
   --  Node. If result is Invalid_Token_Index, all terminals are virtual,
   --  or a nonterm is empty.

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array;

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Token_ID_Array;

   function Image
     (Tree             : in Syntax_Trees.Tree;
      Node             : in Valid_Node_Index;
      Descriptor       : in WisiToken.Descriptor;
      Include_Children : in Boolean := False)
     return String;
   function Image
     (Tree       : in Syntax_Trees.Tree;
      Nodes      : in Valid_Node_Index_Array;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  For debug and error messages.

   procedure Print_Tree (Tree : in Syntax_Trees.Tree; Descriptor : in WisiToken.Descriptor)
   with Pre => Tree.Flushed;
   --  To Text_IO.Current_Output, for debugging.

private

   type Node (Label : Node_Label := Virtual_Terminal) is
   --  Label has a default to allow use with Ada.Containers.Vectors; all
   --  entries are the same size.
   record
      ID : WisiToken.Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Computed by Set_Children, used in Semantic_Check actions and debug
      --  messages.

      Parent : Node_Index := Invalid_Node_Index;

      State : Unknown_State_Index := Unknown_State;
      --  Parse state that was on stack with this token, to allow undoing a
      --  reduce.

      case Label is
      when Shared_Terminal =>
         Terminal : Token_Index;

      when Virtual_Terminal =>
         null;

      when Nonterm =>
         Virtual : Boolean := False;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by Semantic_Check actions.

         RHS_Index : Natural;
         --  With ID, index into Productions.
         --  Used for debug output, keep for future use.

         Action : Semantic_Action := null;

         Name : Buffer_Region := Null_Buffer_Region;
         --  Name is set and checked by Semantic_Check actions.

         Children : Valid_Node_Index_Arrays.Vector;

         Min_Terminal_Index : Base_Token_Index := Invalid_Token_Index;
         --  Cached for push_back of nonterminals during recovery

         Max_Terminal_Index : Base_Token_Index := Invalid_Token_Index;
         --  Cached for building a WisiToken tree from a libadalang tree.

         Augmented : Base_Token_Class_Access := null;
      end case;
   end record;

   subtype Nonterm_Node is Node (Nonterm);

   package Node_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Node);

   type Base_Tree is new Ada.Finalization.Controlled with record
      Nodes : Node_Arrays.Vector;
      --  During normal parsing, tokens are added to Nodes by "parallel"
      --  LALR parsers, but they are all run from one Ada task, so there's
      --  no need for Nodes to be Protected. Packrat parsing also has a
      --  single Ada task.
      --
      --  During McKenzie_Recover, the syntax tree is not modified.

      Augmented_Present : Boolean := False;
      --  True if Set_Augmented has been called on any node.
      --  Declared in Base_Tree because used by Base_Tree.Adjust.

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.
      --  Declared in Base_Tree so it is cleared by Finalize.

   end record;

   type Tree is tagged record
      Shared_Tree : Base_Tree_Access;
      --  If we need to set anything (ie parent) in Shared_Tree, we move the
      --  branch point instead, unless Flush = True.

      Last_Shared_Node : Node_Index := Invalid_Node_Index;
      Branched_Nodes   : Node_Arrays.Vector;
      Flush            : Boolean    := False;
      --  We maintain Last_Shared_Node when Flush is True, so subprograms
      --  that have no reason to check Flush can rely on Last_Shared_Node.

      Root : Node_Index := Invalid_Node_Index;
   end record with
     Type_Invariant => (if Tree.Flush then not Tree.Has_Branched_Nodes);

end WisiToken.Syntax_Trees;
