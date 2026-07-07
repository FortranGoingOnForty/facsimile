module server_detection_module
    use platform_module, only: detect_system_pkg_mgr, detect_priv_prefix
    implicit none
    private

    public :: detected_server_t
    public :: detect_all_servers, check_server_installed
    public :: get_known_servers_count

    integer, parameter :: MAX_SERVERS = 30

    type :: detected_server_t
        character(len=64) :: name = ''           ! "pyright", "clangd", etc.
        character(len=32) :: language = ''       ! "python", "c", etc.
        character(len=256) :: install_cmd = ''   ! "pip install pyright"
        character(len=256) :: description = ''   ! "Python type checker and language server"
        character(len=128) :: check_cmd = ''     ! Command to check if installed
        logical :: is_installed = .false.        ! Result of detection
        character(len=64) :: version = ''        ! Detected version (if installed)
    end type detected_server_t

contains

    function get_known_servers_count() result(count)
        integer :: count
        count = 20  ! Number of servers we know about
    end function get_known_servers_count

    subroutine detect_all_servers(servers, num_servers)
        type(detected_server_t), intent(out), allocatable :: servers(:)
        integer, intent(out) :: num_servers
        integer :: i

        num_servers = get_known_servers_count()
        allocate(servers(num_servers))

        ! Initialize all known servers
        call init_known_servers(servers)

        ! Check which are installed
        do i = 1, num_servers
            servers(i)%is_installed = check_server_installed(servers(i)%check_cmd)
        end do
    end subroutine detect_all_servers

    subroutine init_known_servers(servers)
        type(detected_server_t), intent(inout) :: servers(:)
        integer :: i
        character(len=:), allocatable :: mgr, priv

        ! Detect the system package manager and privilege prefix once so
        ! native-package servers resolve to the right command for this host.
        mgr = detect_system_pkg_mgr()
        priv = detect_priv_prefix()

        i = 1

        ! Python - Pyright
        servers(i)%name = 'pyright'
        servers(i)%language = 'Python'
        servers(i)%install_cmd = 'pipx install pyright'
        servers(i)%description = 'Python type checker and language server'
        servers(i)%check_cmd = 'pyright-langserver'
        i = i + 1

        ! Python - Ruff
        servers(i)%name = 'ruff'
        servers(i)%language = 'Python'
        servers(i)%install_cmd = 'pipx install ruff'
        servers(i)%description = 'Fast Python linter with auto-fix'
        servers(i)%check_cmd = 'ruff'
        i = i + 1

        ! Python - python-lsp-server
        servers(i)%name = 'python-lsp-server'
        servers(i)%language = 'Python'
        servers(i)%install_cmd = 'pipx install python-lsp-server'
        servers(i)%description = 'Python LSP (pylsp) with plugins support'
        servers(i)%check_cmd = 'pylsp'
        i = i + 1

        ! C/C++ - clangd
        servers(i)%name = 'clangd'
        servers(i)%language = 'C/C++'
        servers(i)%install_cmd = resolve_native_cmd('clangd', mgr, priv)
        servers(i)%description = 'C/C++ language server from LLVM'
        servers(i)%check_cmd = 'clangd'
        i = i + 1

        ! Rust - rust-analyzer
        servers(i)%name = 'rust-analyzer'
        servers(i)%language = 'Rust'
        servers(i)%install_cmd = 'rustup component add rust-analyzer'
        servers(i)%description = 'Rust language server'
        servers(i)%check_cmd = 'rust-analyzer'
        i = i + 1

        ! Go - gopls
        servers(i)%name = 'gopls'
        servers(i)%language = 'Go'
        servers(i)%install_cmd = 'go install golang.org/x/tools/gopls@latest'
        servers(i)%description = 'Go language server'
        servers(i)%check_cmd = 'gopls'
        i = i + 1

        ! TypeScript/JavaScript - typescript-language-server
        servers(i)%name = 'typescript-language-server'
        servers(i)%language = 'JS/TS'
        servers(i)%install_cmd = 'npm install -g typescript-language-server typescript'
        servers(i)%description = 'JavaScript and TypeScript language server'
        servers(i)%check_cmd = 'typescript-language-server'
        i = i + 1

        ! JavaScript/TypeScript - vtsls (faster alternative)
        servers(i)%name = 'vtsls'
        servers(i)%language = 'JS/TS'
        servers(i)%install_cmd = 'npm install -g @vtsls/language-server'
        servers(i)%description = 'Fast TypeScript language server'
        servers(i)%check_cmd = 'vtsls'
        i = i + 1

        ! Lua - lua-language-server
        servers(i)%name = 'lua-language-server'
        servers(i)%language = 'Lua'
        servers(i)%install_cmd = &
            resolve_native_cmd('lua-language-server', mgr, priv)
        servers(i)%description = 'Lua language server by sumneko'
        servers(i)%check_cmd = 'lua-language-server'
        i = i + 1

        ! Ruby - solargraph
        servers(i)%name = 'solargraph'
        servers(i)%language = 'Ruby'
        servers(i)%install_cmd = 'gem install solargraph'
        servers(i)%description = 'Ruby language server'
        servers(i)%check_cmd = 'solargraph'
        i = i + 1

        ! Java - jdtls
        servers(i)%name = 'jdtls'
        servers(i)%language = 'Java'
        servers(i)%install_cmd = resolve_native_cmd('jdtls', mgr, priv)
        servers(i)%description = 'Eclipse JDT Language Server'
        servers(i)%check_cmd = 'jdtls'
        i = i + 1

        ! HTML/CSS/JSON - vscode-langservers-extracted
        servers(i)%name = 'vscode-langservers'
        servers(i)%language = 'HTML/CSS/JSON'
        servers(i)%install_cmd = 'npm i -g vscode-langservers-extracted'
        servers(i)%description = 'HTML, CSS, JSON language servers'
        servers(i)%check_cmd = 'vscode-html-language-server'
        i = i + 1

        ! Bash - bash-language-server
        servers(i)%name = 'bash-language-server'
        servers(i)%language = 'Bash'
        servers(i)%install_cmd = 'npm i -g bash-language-server'
        servers(i)%description = 'Bash/Shell language server'
        servers(i)%check_cmd = 'bash-language-server'
        i = i + 1

        ! YAML - yaml-language-server
        servers(i)%name = 'yaml-language-server'
        servers(i)%language = 'YAML'
        servers(i)%install_cmd = 'npm i -g yaml-language-server'
        servers(i)%description = 'YAML language server'
        servers(i)%check_cmd = 'yaml-language-server'
        i = i + 1

        ! Docker - dockerfile-language-server
        servers(i)%name = 'dockerfile-langserver'
        servers(i)%language = 'Docker'
        servers(i)%install_cmd = 'npm i -g dockerfile-language-server-nodejs'
        servers(i)%description = 'Dockerfile language server'
        servers(i)%check_cmd = 'docker-langserver'
        i = i + 1

        ! Zig - zls
        servers(i)%name = 'zls'
        servers(i)%language = 'Zig'
        servers(i)%install_cmd = resolve_native_cmd('zls', mgr, priv)
        servers(i)%description = 'Zig language server'
        servers(i)%check_cmd = 'zls'
        i = i + 1

        ! Swift - sourcekit-lsp
        servers(i)%name = 'sourcekit-lsp'
        servers(i)%language = 'Swift'
        servers(i)%install_cmd = &
            resolve_native_cmd('sourcekit-lsp', mgr, priv)
        servers(i)%description = 'Swift/Obj-C language server'
        servers(i)%check_cmd = 'sourcekit-lsp'
        i = i + 1

        ! Haskell - haskell-language-server
        servers(i)%name = 'haskell-language-server'
        servers(i)%language = 'Haskell'
        servers(i)%install_cmd = 'ghcup install hls'
        servers(i)%description = 'Haskell language server'
        servers(i)%check_cmd = 'haskell-language-server-wrapper'
        i = i + 1

        ! Terraform - terraform-ls
        servers(i)%name = 'terraform-ls'
        servers(i)%language = 'Terraform'
        servers(i)%install_cmd = &
            resolve_native_cmd('terraform-ls', mgr, priv)
        servers(i)%description = 'Terraform language server'
        servers(i)%check_cmd = 'terraform-ls'
        i = i + 1

        ! Fortran - fortls
        servers(i)%name = 'fortls'
        servers(i)%language = 'Fortran'
        servers(i)%install_cmd = 'pipx install fortls'
        servers(i)%description = 'Fortran language server'
        servers(i)%check_cmd = 'fortls'
    end subroutine init_known_servers

    function check_server_installed(cmd_name) result(installed)
        character(len=*), intent(in) :: cmd_name
        logical :: installed
        character(len=512) :: check_command
        integer :: exit_status, cmd_status

        installed = .false.
        if (len_trim(cmd_name) == 0) return

        ! POSIX `command -v` (portable across Linux/BSD/macOS shells).
        ! cmdstat is mandatory: on FreeBSD /bin/sh `command -v <missing>`
        ! exits 127, which aborts execute_command_line unless cmdstat is
        ! present. Treat any non-zero cmdstat/exit as "not installed".
        check_command = 'command -v ' // trim(cmd_name) // ' > /dev/null 2>&1'

        cmd_status = 0
        call execute_command_line(trim(check_command), wait=.true., &
            exitstat=exit_status, cmdstat=cmd_status)

        installed = (cmd_status == 0 .and. exit_status == 0)
    end function check_server_installed

    ! Build a system-package install command for the detected manager.
    ! The privilege prefix is applied to every manager except brew, which
    ! refuses to run as root.
    function pkg_cmd(mgr, priv, pkg_name) result(cmd)
        character(len=*), intent(in) :: mgr, priv, pkg_name
        character(len=:), allocatable :: cmd

        select case (trim(mgr))
        case ('brew')
            cmd = 'brew install ' // trim(pkg_name)
        case ('apt')
            cmd = priv //'apt install -y ' // trim(pkg_name)
        case ('dnf')
            cmd = priv //'dnf install -y ' // trim(pkg_name)
        case ('yum')
            cmd = priv //'yum install -y ' // trim(pkg_name)
        case ('pacman')
            cmd = priv //'pacman -S --noconfirm ' // trim(pkg_name)
        case ('zypper')
            cmd = priv //'zypper install -y ' // trim(pkg_name)
        case ('apk')
            cmd = priv //'apk add ' // trim(pkg_name)
        case ('pkg')
            cmd = priv //'pkg install -y ' // trim(pkg_name)
        case ('pkg_add')
            cmd = priv //'pkg_add ' // trim(pkg_name)
        case ('xbps')
            cmd = priv //'xbps-install -Sy ' // trim(pkg_name)
        case default
            cmd = ''
        end select
    end function pkg_cmd

    ! Resolve a native-package server to the right command for this host.
    ! Package names differ per manager; where no native package exists the
    ! result is a '# ...' hint pointing at the upstream install page rather
    ! than a command that would fail.
    function resolve_native_cmd(name, mgr, priv) result(cmd)
        character(len=*), intent(in) :: name, mgr, priv
        character(len=:), allocatable :: cmd
        character(len=:), allocatable :: pkg

        cmd = ''

        select case (trim(name))
        case ('clangd')
            select case (trim(mgr))
            case ('brew', 'pkg', 'pkg_add')
                pkg = 'llvm'
            case ('apt')
                pkg = 'clangd'
            case ('dnf', 'yum', 'xbps')
                pkg = 'clang-tools-extra'
            case ('pacman')
                pkg = 'clang'
            case ('zypper')
                pkg = 'clang-tools'
            case ('apk')
                pkg = 'clang-extra-tools'
            case default
                pkg = ''
            end select
            if (len(pkg) > 0) cmd = pkg_cmd(mgr, priv, pkg)
            if (len_trim(cmd) == 0) &
                cmd = '# see releases.llvm.org (install clangd)'

        case ('lua-language-server')
            select case (trim(mgr))
            case ('brew', 'dnf', 'pacman', 'pkg')
                cmd = pkg_cmd(mgr, priv, 'lua-language-server')
            case default
                cmd = '# see github.com/LuaLS/lua-language-server'
            end select

        case ('zls')
            if (trim(mgr) == 'brew') then
                cmd = pkg_cmd(mgr, priv, 'zls')
            else
                cmd = '# see github.com/zigtools/zls/releases'
            end if

        case ('jdtls')
            if (trim(mgr) == 'brew') then
                cmd = pkg_cmd(mgr, priv, 'jdtls')
            else
                cmd = '# see github.com/eclipse-jdtls/eclipse.jdt.ls'
            end if

        case ('terraform-ls')
            if (trim(mgr) == 'brew') then
                cmd = 'brew install hashicorp/tap/terraform-ls'
            else
                cmd = &
                    '# see developer.hashicorp.com/terraform/language-server'
            end if

        case ('sourcekit-lsp')
            if (trim(mgr) == 'brew') then
                cmd = '# included with Xcode'
            else
                cmd = &
                    '# included with the Swift toolchain (swift.org/install)'
            end if

        case default
            cmd = ''
        end select
    end function resolve_native_cmd

end module server_detection_module
