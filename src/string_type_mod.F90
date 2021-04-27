module string_type_mod

  implicit none

  private

  type, public :: string
    private
    character(len=:), allocatable :: value
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

    procedure, public, pass(self)  :: get_value => get_string_value
    procedure, public, pass(self)  :: set_value => set_string_value
    procedure, public, pass(self)  :: len       => len_string
    procedure, public, pass(self)  :: len_trim  => len_trim_string
    procedure, public, pass(self)  :: trim      => trim_string
    procedure, public, pass(self)  :: adjustl   => adjustl_string
    procedure, public, pass(self)  :: adjustr   => adjustr_string
    procedure, public, pass(self)  :: reverse   => resverse_string
    procedure, public, pass(self)  :: to_lower  => to_lower_string
    procedure, public, pass(self)  :: to_upper  => to_upper_string
    procedure, public, pass(self)  :: to_int    => string_to_int
    procedure, public, pass(self)  :: capitalize
    procedure, public, pass(self)  :: count     => count_substring
    procedure, public, pass(self)  :: find
    procedure, public, pass(self)  :: start_with
    procedure, public, pass(self)  :: end_with

#ifdef __GNUC__
    procedure, public, pass(self)  :: delete => delete_string_polymorph
#endif
    final :: delete_string
  end type string

  interface string
    module procedure new_string_from_character
    module procedure new_string_from_integer
  end interface string

contains

  function new_string_from_character(val) result(new)

    implicit none
    character(len=*), intent(in) :: val
    type(string)                 :: new

    new%value = val

  end function new_string_from_character

  function new_string_from_integer(val) result(new)

    implicit none
    integer, intent(in)       :: val
    type(string)              :: new
    integer, parameter        :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer

    write(buffer, '(i0)') val
    new%value = trim(buffer)

  end function new_string_from_integer

  subroutine delete_string(self)

    implicit none
    type(string), intent(inout) :: self

    if (allocated(self%value)) then
      deallocate(self%value)
    end if

  end subroutine delete_string

#ifdef __GNUC__
  subroutine delete_string_polymorph(self)

    implicit none
    class(string), intent(inout) :: self

    if (allocated(self%value)) then
      deallocate(self%value)
    end if

  end subroutine delete_string_polymorph
#endif

!------ assignment(=) procedures start
  subroutine assign_string_to_string(lhs, rhs)

    implicit none
    class(string), intent(inout) :: lhs
    type(string), intent(in)     :: rhs

    lhs%value = rhs%value

  end subroutine assign_string_to_string

  subroutine assign_character_to_string(lhs, rhs)

    implicit none
    class(string), intent(inout) :: lhs
    character(len=*), intent(in) :: rhs

    lhs%value = rhs

  end subroutine assign_character_to_string

  subroutine assign_string_to_character(lhs, rhs)

    implicit none
    character(len=:), allocatable, intent(out) :: lhs
    class(string), intent(in)                  :: rhs

    lhs = rhs%value

  end subroutine assign_string_to_character
!------ assignment(=) procedures end

!------ operator(//) procedures start
  function string_concat_string(lhs, rhs) result(concat)

    implicit none
    class(string), intent(in) :: lhs
    type(string), intent(in)  :: rhs
    type(string)              :: concat

    concat = lhs%value//rhs%value

  end function string_concat_string

  function string_concat_character(lhs, rhs) result(concat)

    implicit none
    class(string), intent(in)    :: lhs
    character(len=*), intent(in) :: rhs
    type(string)                 :: concat

    concat = lhs%value//rhs

  end function string_concat_character

  function character_concat_string(lhs, rhs) result(concat)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string), intent(in)    :: rhs
    type(string)                 :: concat

    concat = lhs//rhs%value

  end function character_concat_string
!------ operator(//) procedures end

!------ operator(==) procedures start
  function string_eq_string(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in) :: lhs
    type(string), intent(in)  :: rhs
    logical                   :: equal

    equal = lhs%value == rhs%value

  end function string_eq_string

  function string_eq_character(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in)    :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value == rhs

  end function string_eq_character

  function character_eq_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string), intent(in)    :: rhs
    logical                      :: equal

    equal = lhs == rhs%value

  end function character_eq_string
!------ operator(==) procedures end

!------ operator(/=) procedures start
  function string_ne_string(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in) :: lhs
    type(string), intent(in)  :: rhs
    logical                   :: equal

    equal = lhs%value /= rhs%value

  end function string_ne_string

  function string_ne_character(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in)    :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value /= rhs

  end function string_ne_character

  function character_ne_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string), intent(in)    :: rhs
    logical                      :: equal

    equal = lhs /= rhs%value

  end function character_ne_string
!------ operator(/=) procedures end

!------ operator(>) procedures start
  function string_gt_string(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in) :: lhs
    type(string), intent(in)  :: rhs
    logical                   :: equal

    equal = lhs%value > rhs%value

  end function string_gt_string

  function string_gt_character(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in)    :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value > rhs

  end function string_gt_character

  function character_gt_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string), intent(in)    :: rhs
    logical                      :: equal

    equal = lhs > rhs%value

  end function character_gt_string
!------ operator(>) procedures end

!------ operator(>=) procedures start
  function string_ge_string(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in) :: lhs
    type(string), intent(in)  :: rhs
    logical                   :: equal

    equal = lhs%value >= rhs%value

  end function string_ge_string

  function string_ge_character(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in)    :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value >= rhs

  end function string_ge_character

  function character_ge_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string), intent(in)    :: rhs
    logical                      :: equal

    equal = lhs >= rhs%value

  end function character_ge_string
!------ operator(>=) procedures end

!------ operator(<) procedures start
  function string_lt_string(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in) :: lhs
    type(string), intent(in)  :: rhs
    logical                   :: equal

    equal = lhs%value < rhs%value

  end function string_lt_string

  function string_lt_character(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in)    :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value < rhs

  end function string_lt_character

  function character_lt_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string), intent(in)    :: rhs
    logical                      :: equal

    equal = lhs < rhs%value

  end function character_lt_string
!------ operator(<) procedures end

!------ operator(<=) procedures start
  function string_le_string(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in) :: lhs
    type(string), intent(in)  :: rhs
    logical                   :: equal

    equal = lhs%value <= rhs%value

  end function string_le_string

  function string_le_character(lhs, rhs) result(equal)

    implicit none
    class(string), intent(in)    :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equal

    equal = lhs%value <= rhs

  end function string_le_character

  function character_le_string(lhs, rhs) result(equal)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string), intent(in)    :: rhs
    logical                      :: equal

    equal = lhs <= rhs%value

  end function character_le_string
!------ operator(<=) procedures end

  function get_string_value(self) result(string_value)

    implicit none
    class(string), intent(in)     :: self
    character(len=:), allocatable :: string_value

    string_value = self%value

  end function get_string_value

  subroutine set_string_value(self, string_value)

    implicit none
    class(string), intent(inout) :: self
    character(len=*), intent(in) :: string_value

    self%value = trim(string_value)

  end subroutine set_string_value

  function len_string(self) result(length)

    implicit none
    class(string), intent(in) :: self
    integer                   :: length

    length = len(self%value)

  end function len_string

  function len_trim_string(self) result(length)

    implicit none
    class(string), intent(in) :: self
    integer                   :: length

    length = len_trim(self%value)

  end function len_trim_string

  function trim_string(self) result(trimmed_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: trimmed_string

    trimmed_string = trim(self%value)

  end function trim_string

  function adjustl_string(self) result(adjusted_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: adjusted_string

    adjusted_string = adjustl(self%value)

  end function adjustl_string

  function adjustr_string(self) result(adjusted_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: adjusted_string

    adjusted_string = adjustr(self%value)

  end function adjustr_string

  function resverse_string(self) result(reversed_string)

    implicit none
    class(string), intent(in) :: self
    type(string)              :: reversed_string
    integer                   :: i, n

    reversed_string = self
    n = self%len()
    do i = 1, n
      reversed_string%value(n-i+1:n-i+1) = self%value(i:i)
    end do

  end function resverse_string

  function to_lower_string(self) result(lower_string)

    implicit none
    class(string), intent(in)   :: self
    type(string)                :: lower_string
    integer                     :: i

    lower_string = self
    do i = 1, self%len()
      select case (lower_string%value(i:i))
      case ('A':'Z')
        lower_string%value(i:i) = char(iachar(lower_string%value(i:i))+32)
      case default
      end select
    end do

  end function to_lower_string

function to_upper_string(self) result(upper_string)

    implicit none
    class(string), intent(in)   :: self
    type(string)                :: upper_string
    integer                     :: i

    upper_string = self
    do i = 1, self%len()
      select case (upper_string%value(i:i))
      case ('a':'z')
        upper_string%value(i:i) = char(iachar(upper_string%value(i:i))-32)
      case default
      end select
    end do

  end function to_upper_string

  function capitalize(self) result(capitalized_string)

    implicit none
    class(string), intent(in)   :: self
    type(string)                :: capitalized_string

    capitalized_string = self%to_lower()
    select case (capitalized_string%value(1:1))
    case ('a':'z')
      capitalized_string%value(1:1) = char(iachar(capitalized_string%value(1:1))-32)
    case default
    end select

  end function capitalize

  function count_substring(self, substring) result(number)

    implicit none
    class(string), intent(in) :: self
    character(*), intent(in)  :: substring
    integer                   :: number
    integer                   :: start, idx

    number = 0
    if (len(substring) < self%len()) then
      start = 1
      do
        idx = index(self%value(start:), substring)
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
    class(string), intent(in)     :: self
    character(*), intent(in)      :: substring
    logical, intent(in), optional :: back
    integer                       :: idx

    if (len(substring) < self%len()) then
      idx = index(self%value, substring, back)
    else
      idx = 0
    end if

  end function find

  function start_with(self, prefix, start, end) result(res)

    implicit none
    class(string), intent(in)     :: self
    character(*), intent(in)      :: prefix
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

    res = index(self%value(start_:end_), prefix) == 1

  end function start_with

  function end_with(self, suffix, start, end) result(res)

    implicit none
    class(string), intent(in)     :: self
    character(*), intent(in)      :: suffix
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

    res = self%value(end_-len(suffix)+1:end_) == suffix

  end function end_with

  function string_to_int(self) result(int)

    implicit none
    class(string), intent(in) :: self
    integer                   :: int

    read(self%value, *) int

  end function string_to_int

end module string_type_mod