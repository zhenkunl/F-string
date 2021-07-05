module string_type_mod
  use iso_fortran_env, only : int8, int16, int32, int64, real32, real64

  implicit none
  private
  public :: string_t

  type :: string_t
    private
    character(len=:), allocatable  :: value_
  contains
    private
    generic, public                :: assignment(=) => assign_string_to_string, assign_character_to_string, &
                                      assign_string_to_character
    procedure, private, pass(lhs)  :: assign_string_to_string
    procedure, private, pass(lhs)  :: assign_character_to_string
    procedure, private, pass(rhs)  :: assign_string_to_character

    generic, public                :: operator(//) => string_concat_string, string_concat_character,        &
                                      character_concat_string
    procedure, private, pass(lhs)  :: string_concat_string
    procedure, private, pass(lhs)  :: string_concat_character
    procedure, private, pass(rhs)  :: character_concat_string

    generic, public                :: operator(==) => string_eq_string, string_eq_character,                &
                                      character_eq_string
    procedure, private, pass(lhs)  :: string_eq_string
    procedure, private, pass(lhs)  :: string_eq_character
    procedure, private, pass(rhs)  :: character_eq_string

    generic, public                :: operator(/=) => string_ne_string, string_ne_character,                &
                                      character_ne_string
    procedure, private, pass(lhs)  :: string_ne_string
    procedure, private, pass(lhs)  :: string_ne_character
    procedure, private, pass(rhs)  :: character_ne_string

    generic, public                :: operator(>) => string_gt_string, string_gt_character,                 &
                                      character_gt_string
    procedure, private, pass(lhs)  :: string_gt_string
    procedure, private, pass(lhs)  :: string_gt_character
    procedure, private, pass(rhs)  :: character_gt_string

    generic, public                :: operator(>=) => string_ge_string, string_ge_character,                &
                                      character_ge_string
    procedure, private, pass(lhs)  :: string_ge_string
    procedure, private, pass(lhs)  :: string_ge_character
    procedure, private, pass(rhs)  :: character_ge_string

    generic, public                :: operator(<) => string_lt_string, string_lt_character,                 &
                                      character_lt_string
    procedure, private, pass(lhs)  :: string_lt_string
    procedure, private, pass(lhs)  :: string_lt_character
    procedure, private, pass(rhs)  :: character_lt_string

    generic, public                :: operator(<=) => string_le_string, string_le_character,                &
                                      character_le_string
    procedure, private, pass(lhs)  :: string_le_string
    procedure, private, pass(lhs)  :: string_le_character
    procedure, private, pass(rhs)  :: character_le_string

    procedure, public, pass(self)  :: value    => get_string_value
    procedure, public, pass(self)  :: len      => len_string
    procedure, public, pass(self)  :: len_trim => len_trim_string
    procedure, public, pass(self)  :: trim     => trim_string
    procedure, public, pass(self)  :: adjustl  => adjustl_string
    procedure, public, pass(self)  :: adjustr  => adjustr_string
    procedure, public, pass(self)  :: reverse  => resverse_string
    procedure, public, pass(self)  :: to_lower => to_lower_string
    procedure, public, pass(self)  :: to_upper => to_upper_string
    procedure, public, pass(self)  :: capitalize
    procedure, public, pass(self)  :: count    => count_substring
    procedure, public, pass(self)  :: find
    procedure, public, pass(self)  :: start_with
    procedure, public, pass(self)  :: end_with

    final                          :: string_finalize
  end type string_t

  interface string_t
    module procedure constructor_from_int8
    module procedure constructor_from_int16
    module procedure constructor_from_int32
    module procedure constructor_from_int64
    module procedure constructor_from_real32
    module procedure constructor_from_real64
    module procedure constructor_from_logical
    module procedure constructor_from_character
  end interface string_t

contains

!------ constructor procedures start
  function constructor_from_int8(value) result(new)

    implicit none
    integer(int8), intent(in)     :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int8

  function constructor_from_int16(value) result(new)

    implicit none
    integer(int16), intent(in)    :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int16

  function constructor_from_int32(value) result(new)

    implicit none
    integer(int32), intent(in)    :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int32

  function constructor_from_int64(value) result(new)

    implicit none
    integer(int64), intent(in)    :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int64

  function constructor_from_real32(value, decimal_width, width) result(new)

    implicit none
    real(real32), intent(in)      :: value
    integer, intent(in), optional :: decimal_width
    integer, intent(in), optional :: width
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer
    character(len=5)              :: format
    integer                       :: total_width

    if (present(decimal_width)) then
      if (present(width)) then
        total_width = max(width, decimal_width + 7)
      else
        total_width = decimal_width + 7
      end if
      write(format, '(a1, i0, a1, i0)') 'G', total_width, '.', decimal_width
      write(buffer, '(' // format // ')') value
      new = trim(adjustl(buffer))
    else
      write(buffer, *) value
      new = trim(adjustl(buffer))
    end if

  end function constructor_from_real32

  function constructor_from_real64(value, decimal_width, width) result(new)

    implicit none
    real(real64), intent(in)      :: value
    integer, intent(in), optional :: decimal_width
    integer, intent(in), optional :: width
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer
    character(len=5)              :: format
    integer                       :: total_width

    if (present(decimal_width)) then
      if (present(width)) then
        total_width = max(width, decimal_width + 7)
      else
        total_width = decimal_width + 7
      end if
      write(format, '(a1, i0, a1, i0)') 'G', total_width, '.', decimal_width
      write(buffer, '(' // format // ')') value
      new = trim(adjustl(buffer))
    else
      write(buffer, *) value
      new = trim(adjustl(buffer))
    end if

  end function constructor_from_real64

  function constructor_from_logical(value) result(new)

    implicit none
    logical, intent(in) :: value
    type(string_t)      :: new

    if (value) then
      new = "True"
    else
      new = "False"
    end if

  end function constructor_from_logical

  function constructor_from_character(value) result(new)

    implicit none
    character(len=*), intent(in) :: value
    type(string_t)               :: new

    new = value

  end function constructor_from_character
!------ constructor procedures end

  subroutine string_finalize(self)

    implicit none
    type(string_t), intent(inout) :: self

    if (allocated(self%value_)) then
      deallocate(self%value_)
    end if

  end subroutine string_finalize

!------ assignment(=) procedures start
  subroutine assign_string_to_string(lhs, rhs)

    implicit none
    class(string_t), intent(inout) :: lhs
    type(string_t), intent(in)     :: rhs

    lhs%value_ = rhs%value_

  end subroutine assign_string_to_string

  subroutine assign_character_to_string(lhs, rhs)

    implicit none
    class(string_t), intent(inout) :: lhs
    character(len=*), intent(in) :: rhs

    lhs%value_ = rhs

  end subroutine assign_character_to_string

  subroutine assign_string_to_character(lhs, rhs)

    implicit none
    character(len=:), allocatable, intent(out) :: lhs
    class(string_t), intent(in)                :: rhs

    lhs = rhs%value_

  end subroutine assign_string_to_character
!------ assignment(=) procedures end

!------ operator(//) procedures start
  function string_concat_string(lhs, rhs) result(concat_string)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    type(string_t)              :: concat_string

    concat_string = lhs%value_//rhs%value_

  end function string_concat_string

  function string_concat_character(lhs, rhs) result(concat_string)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    type(string_t)               :: concat_string

    concat_string = lhs%value_//rhs

  end function string_concat_character

  function character_concat_string(lhs, rhs) result(concat_string)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    type(string_t)               :: concat_string

    concat_string = lhs//rhs%value_

  end function character_concat_string
!------ operator(//) procedures end

!------ operator(==) procedures start
  function string_eq_string(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equal

    equal = lhs%value_ == rhs%value_

  end function string_eq_string

  function string_eq_character(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value_ == rhs

  end function string_eq_character

  function character_eq_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equal

    equal = lhs == rhs%value_

  end function character_eq_string
!------ operator(==) procedures end

!------ operator(/=) procedures start
  function string_ne_string(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equal

    equal = lhs%value_ /= rhs%value_

  end function string_ne_string

  function string_ne_character(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value_ /= rhs

  end function string_ne_character

  function character_ne_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equal

    equal = lhs /= rhs%value_

  end function character_ne_string
!------ operator(/=) procedures end

!------ operator(>) procedures start
  function string_gt_string(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equal

    equal = lhs%value_ > rhs%value_

  end function string_gt_string

  function string_gt_character(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value_ > rhs

  end function string_gt_character

  function character_gt_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equal

    equal = lhs > rhs%value_

  end function character_gt_string
!------ operator(>) procedures end

!------ operator(>=) procedures start
  function string_ge_string(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equal

    equal = lhs%value_ >= rhs%value_

  end function string_ge_string

  function string_ge_character(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value_ >= rhs

  end function string_ge_character

  function character_ge_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equal

    equal = lhs >= rhs%value_

  end function character_ge_string
!------ operator(>=) procedures end

!------ operator(<) procedures start
  function string_lt_string(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equal

    equal = lhs%value_ < rhs%value_

  end function string_lt_string

  function string_lt_character(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value_ < rhs

  end function string_lt_character

  function character_lt_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equal

    equal = lhs < rhs%value_

  end function character_lt_string
!------ operator(<) procedures end

!------ operator(<=) procedures start
  function string_le_string(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equal

    equal = lhs%value_ <= rhs%value_

  end function string_le_string

  function string_le_character(lhs, rhs) result(equal)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value_ <= rhs

  end function string_le_character

  function character_le_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equal

    equal = lhs <= rhs%value_

  end function character_le_string
!------ operator(<=) procedures end

  function get_string_value(self) result(string_value)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=:), allocatable :: string_value

    string_value = self%value_

  end function get_string_value

  function len_string(self) result(length)

    implicit none
    class(string_t), intent(in) :: self
    integer                     :: length

    length = len(self%value_)

  end function len_string

  function len_trim_string(self) result(length)

    implicit none
    class(string_t), intent(in) :: self
    integer                     :: length

    length = len_trim(self%value_)

  end function len_trim_string

  function trim_string(self) result(trimmed_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: trimmed_string

    trimmed_string = trim(self%value_)

  end function trim_string

  function adjustl_string(self) result(adjusted_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: adjusted_string

    adjusted_string = adjustl(self%value_)

  end function adjustl_string

  function adjustr_string(self) result(adjusted_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: adjusted_string

    adjusted_string = adjustr(self%value_)

  end function adjustr_string

  function resverse_string(self) result(reversed_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: reversed_string
    integer                     :: i, n

    reversed_string = self
    n = self%len()
    do i = 1, n
      reversed_string%value_(n-i+1:n-i+1) = self%value_(i:i)
    end do

  end function resverse_string

  function to_lower_string(self) result(lower_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: lower_string
    integer                     :: i

    lower_string = self
    do i = 1, self%len()
      select case (lower_string%value_(i:i))
      case ('A':'Z')
        lower_string%value_(i:i) = char(iachar(lower_string%value_(i:i))+32)
      case default
      end select
    end do

  end function to_lower_string

function to_upper_string(self) result(upper_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: upper_string
    integer                     :: i

    upper_string = self
    do i = 1, self%len()
      select case (upper_string%value_(i:i))
      case ('a':'z')
        upper_string%value_(i:i) = achar(iachar(upper_string%value_(i:i))-32)
      case default
      end select
    end do

  end function to_upper_string

  function capitalize(self) result(capitalized_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: capitalized_string

    capitalized_string = self%to_lower()
    select case (capitalized_string%value_(1:1))
    case ('a':'z')
      capitalized_string%value_(1:1) = achar(iachar(capitalized_string%value_(1:1))-32)
    case default
    end select

  end function capitalize

  function count_substring(self, substring) result(number)

    implicit none
    class(string_t), intent(in)  :: self
    character(len=*), intent(in) :: substring
    integer                      :: number
    integer                      :: start, idx

    number = 0
    if (len(substring) < self%len()) then
      start = 1
      do
        idx = index(self%value_(start:), substring)
        if (idx == 0) then
          exit
        else
          number = number + 1
        end if
        start = start + idx + len(substring) - 1
      end do
    end if

  end function count_substring

  function find(self, substring, back) result(idx)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: substring
    logical, intent(in), optional :: back
    integer                       :: idx

    if (len(substring) < self%len()) then
      idx = index(self%value_, substring, back)
    else
      idx = 0
    end if

  end function find

  function start_with(self, prefix, start, end) result(res)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: prefix
    integer, intent(in), optional :: start
    integer, intent(in), optional :: end
    logical                       :: res
    integer                       :: start_, end_

    if (present(start)) then
      start_ = start
    else
      start_ = 1
    end if

    if (present(end)) then
      end_ = end
    else
      end_ = self%len()
    end if

    res = index(self%value_(start_:end_), prefix) == 1

  end function start_with

  function end_with(self, suffix, start, end) result(res)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: suffix
    integer, intent(in), optional :: start
    integer, intent(in), optional :: end
    logical                       :: res
    integer                       :: start_, end_

    if (present(start)) then
      start_ = start
    else
      start_ = 1
    end if

    if (present(end)) then
      end_ = end
    else
      end_ = self%len()
    end if

    res = self%value_(end_-len(suffix)+1:end_) == suffix

  end function end_with

end module string_type_mod