%define	ver	0.17

Summary: file command like perl5 module
Name: File-MMagic
Version: %{ver}
Release: 1
Copyright: distributable
Group: Utilities/File
Source: http://www.perl.com/CPAN/modules/by-module/File/File-MMagic-%{ver}.tar.gz
BuildRoot: /var/tmp/File-MMagic-%{ver}-buildroot/
Requires: perl >= 5.004
Summary(ja): fileコマンド風perl5モジュール

%description
This module is to guess file type from its contents like file(1)
command.

%description -l ja
このモジュールは、file(1)コマンドに相当する機能、すなわちファイルの内
容からその種類を推測するためのものです。


%prep
%setup

%build
CFLAGS="$RPM_OPT_FLAGS" perl Makefile.PL
make

%install
rm -rf $RPM_BUILD_ROOT
eval `perl '-V:installarchlib'`
mkdir -p $RPM_BUILD_ROOT/$installarchlib
make PREFIX=$RPM_BUILD_ROOT/usr install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/lib/perl5/site_perl/File/MMagic.pm
/usr/lib/perl5/site_perl/*-linux/auto/File/MMagic
/usr/lib/perl5/man/man3/File::MMagic.3

%changelog
* Thu Sep 09 1999 Ryuji Abe <raeva@t3.rim.or.jp>
- First build.
