program test_string_type

  use string_type_mod
  implicit none

  type(string) :: str, str2, str3

  print*, '-', str%get_value(), '-', str%len(), str%len_trim()
  str = string("123 ")
  print*, str%get_value(), str%len(), str%len_trim()
  str = string(789)
  print*, str%get_value(), str%len(), str%len_trim()
  str = string(" 4567 ")
  print*, str%get_value(), str%len(), str%len_trim()
  str = " 12 34 "
  print*, str%to_int()
  str2 = str%reverse()
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
#ifdef __GNUC__
  call str%delete()
#endif
end program test_string_type