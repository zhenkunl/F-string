program test_string_type

  use iso_fortran_env, only : i1 => int8, i2 => int16, i4 => int32, i8 => int64, r4 => real32, r8 => real64
  use string_type, only: string_t
  implicit none

  type(string_t) :: str1, str2, str3, str4
  character(:), allocatable :: characters
  integer(i4) :: i32
  real(r4) :: r32
  real(r8) :: r64

  print*, '-', str1%value(), '-', str1%len(), str1%len_trim()
  str1 = string_t("123 ")
  write(*, *) str1%colorize("Red")
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1%at(2, 3)
  i32 = 789
  str1 = string_t(i32)
  print*, str1%value(), str1%len(), str1%len_trim()
  str2 = str1%colorize("BLUE")
  print*, str2%value(), str2%len(), str2%len_trim()
  str1 = string_t(.true.)
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = string_t(0.0123456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "0.123E-01"
  str1 = string_t(-0.0123456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "-0.123E-01"
  str1 = string_t(0.123456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "0.123"
  str1 = string_t(-0.123456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "-0.123"
  str1 = string_t(1.23456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "1.23"
  str1 = string_t(-1.23456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "-1.23"
  str1 = string_t(12.3456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "12.3"
  str1 = string_t(-12.3456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "-12.3"
  str1 = string_t(123.456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "123."
  str1 = string_t(-123.456789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "-123."
  str1 = string_t(1234.56789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "0.123E+04"
  str1 = string_t(-1234.56789, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  print*, str1 == "-0.123E+04"
  str1 = string_t(0.0123456789)
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = string_t(0.123456789)
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = string_t(123.456789)
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = string_t(0.01234)
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = string_t(0.1234)
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = string_t(123.4)
  print*, str1%value(), str1%len(), str1%len_trim()
  r32 = 123.456789
  str1 = string_t(r32, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  r64 = 123.456789
  str1 = string_t(r64, 3)
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = string_t(" 4567 ")
  print*, str1%value(), str1%len(), str1%len_trim()
  str1 = " 12 34 "
  str2 = str1%reverse()
  print*, str2%value(), str2%len(), str2%len_trim()
  str1 = string_t("that Is ")
  str2 = str1%to_lower()
  print*, str2%value(), str2%len(), str2%len_trim()
  str2 = str1%to_upper()
  print*, str2%value(), str2%len(), str2%len_trim()
  str2 = str1%capitalize()
  print*, str2%value(), str2%len(), str2%len_trim()
  str2 = str1%trim()
  print*, str2%value(), str2%len(), str2%len_trim()
  str1 = " 0123456123456 123456123456 "
  print*, str1%count(" "), str1%count("123"), str1%count("61")
  print*, str1%index("34"), str1%index("56", .true.)
  str1 = "123log.txt"
  print*, str1%start_with("123"), str1%start_with("og", 5, 15)
  print*, str1%end_with(".txt"), str1%end_with("txt", 11, 20)
  str1 = "123"
  str2 = "456"
  str3 = "789"
  str4 = str3//"098"
  print*, str4%value(), str4%len(), str4%len_trim()
  str4 = str1//str3
  print*, str4%value(), str4%len(), str4%len_trim()
  str4 = "098"//str3
  print*, str4%value(), str4%len(), str4%len_trim()
  characters = str1
  print*, characters
  print*, str1 == str2, str1 /= str2
  str2 = str1
  print*, str1 == str2, str1 /= str2
  print*, str3 == "789", "789" == str3, str3 /= "789", "789" /= str3
  print*, str3 > "789", "789" > str3, str3 > "788", "788" > str3, str3 >= "789", "789" >= str3
  print*, str3 < "789", "789" < str3, str3 < "790", "790" > str3, str3 <= "789", "789" >= str3
end program test_string_type