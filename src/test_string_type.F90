program test_string_type

  use string_type_mod
  implicit none

  type(string) :: str1, str2, str3, str4
  character(:), allocatable :: characters

  print*, '-', str1%get_value(), '-', str1%len(), str1%len_trim()
  str1 = string("123 ")
  print*, str1%get_value(), str1%len(), str1%len_trim()
  str1 = string(789)
  print*, str1%get_value(), str1%len(), str1%len_trim()
  str1 = string(.true.)
  print*, str1%get_value(), str1%len(), str1%len_trim()
  str1 = string(3.2123456, 2, 5)
  print*, str1%get_value(), str1%len(), str1%len_trim()
  str1 = string(" 4567 ")
  print*, str1%get_value(), str1%len(), str1%len_trim()
  str1 = " 12 34 "
  print*, str1%to_int()
  str2 = str1%reverse()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str1 = string("that Is ")
  str2 = str1%to_lower()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str2 = str1%to_upper()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str2 = str1%capitalize()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str2 = str1%trim()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str1 = " 0123456123456 123456123456 "
  print*, str1%count(" "), str1%count("123"), str1%count("61")
  print*, str1%find("34"), str1%find("56", .true.)
  str1 = "123log.txt"
  print*, str1%start_with("123"), str1%start_with("og", 5, 15)
  print*, str1%end_with(".txt"), str1%end_with("txt", 11, 20)
  str1 = "123"
  str2 = "456"
  str3 = "789"
  str4 = str3//"098"
  print*, str4%get_value(), str4%len(), str4%len_trim()
  str4 = str1//str3
  print*, str4%get_value(), str4%len(), str4%len_trim()
  str4 = "098"//str3
  print*, str4%get_value(), str4%len(), str4%len_trim()
  characters = str1
  print*, characters
  print*, str1 == str2, str1 /= str2
  str2 = str1
  print*, str1 == str2, str1 /= str2
  print*, str3 == "789", "789" == str3, str3 /= "789", "789" /= str3
  print*, str3 > "789", "789" > str3, str3 > "788", "788" > str3, str3 >= "789", "789" >= str3
  print*, str3 < "789", "789" < str3, str3 < "790", "790" > str3, str3 <= "789", "789" >= str3
#ifdef __GNUC__
  call str1%delete()
  call str2%delete()
  call str3%delete()
  call str4%delete()
#endif
end program test_string_type