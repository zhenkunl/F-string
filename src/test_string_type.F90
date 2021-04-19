program test_string_type

  use string_type_mod
  implicit none

  type(string) :: str, str2
  character(len=:), allocatable :: str3

  print*, '-', str%get_value(), '-', str%len(), str%len_trim()
  str = string("123 ")
  print*, str%get_value(), str%len(), str%len_trim()
  str = string(789)
  print*, str%get_value(), str%len(), str%len_trim()
  str = string(" 4567 ")
  print*, str%get_value(), str%len(), str%len_trim()
  str = " 12 34 "
  print*, str%to_int()
  print*, str%reverse()
  print*, str%get_value(), str%len(), str%len_trim()
  str2 = string("That Is ")
  print*, str2%to_lower(), str2%to_upper()
  print*, str2%get_value(), str2%len(), str2%len_trim()
  str = str2
  print*, str%get_value(), str%len(), str%len_trim()
  str3 = str%trim()
  print*, str3, len(str3), len_trim(str3)
#ifdef __GNUC__
  call str%delete()
#endif
end program test_string_type