with Ada.Text_IO; use Ada.Text_IO;

procedure Game_Interface is
   type Color_Scheme is (Default, Dark_Mode, Light_Mode, Custom);
   
   function Choose_Color_Scheme return Color_Scheme is
      Selection : Integer;
   begin
      loop
         Put_Line("Select a color scheme:");
         Put_Line("1. Default");
         Put_Line("2. Dark Mode");
         Put_Line("3. Light Mode");
         Put_Line("4. Custom");
         Put("Enter your choice: ");
         Get_Line(Selection);
         
         case Selection is
            when 1 =>
               return Default;
            when 2 =>
               return Dark_Mode;
            when 3 =>
               return Light_Mode;
            when 4 =>
               return Custom;
            when others =>
               Put_Line("Invalid choice. Please select again.");
         end case;
      end loop;
   end Choose_Color_Scheme;
   
   procedure Apply_Color_Scheme(Scheme : Color_Scheme) is
   begin
      case Scheme is
         when Default =>
            null;
         when Dark_Mode =>
            null;
         when Light_Mode =>
            null;
         when Custom =>
            null;
      end case;
   end Apply_Color_Scheme;
   
begin
   Put_Line("Welcome to the Game!");
   declare
      Chosen_Scheme : Color_Scheme;
   begin
      Chosen_Scheme := Choose_Color_Scheme;
      Apply_Color_Scheme(Chosen_Scheme);
   end;
end Game_Interface;
