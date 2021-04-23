program test_string_type

  use string_type_mod
  implicit none

  type(string) :: str, str1, str2, str3, str4
  character(*), parameter :: chars(3) = ["123", "456", "789"]
  type(string) :: strs(3)

  print*, '-', str%get_value(), '-', str%len(), str%len_trim()
  str = string("123 ")
  print*, str%get_value(), str%len(), str%len_trim()
  str = string(789)
  print*, str%get_value(), str%len(), str%len_trim()
  str = string(" 4567 ")
  print*, str%get_value(), str%len(), str%len_trim()
  str1 = " 12 34 "
  print*, str1%to_int()
  str2 = str1%reverse()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str = string("that Is ")
  str2 = str%to_lower()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str2 = str%to_upper()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str2 = str%capitalize()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str3 = str%trim()
  print*, str3%get_value(), str3%len(), str3%len_trim()
  str = " 0123456123456 123456123456 "
  print*, str%count(" "), str%count("123"), str%count("61")
  print*, str%find("34"), str%find("56", .true.)
  str = "123log.txt"
  print*, str%start_with("123"), str%start_with("og", 5, 15)
  print*, str%end_with(".txt"), str%end_with("txt", 11, 20)
  str = '-'
  str4 = str%join(chars)
  print*, str4%get_value(), str4%len(), str4%len_trim()
  strs(1) = str1
  strs(2) = str2
  strs(3) = str3
  str4 = str%join(strs)
  print*, str4%get_value(), str4%len(), str4%len_trim()
  print*, str3//"098", str1//str3, "098"//str3
#ifdef __GNUC__
  call str%delete()
#endif
end program test_string_type