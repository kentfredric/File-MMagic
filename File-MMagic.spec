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
Summary(ja): file���ޥ����perl5�⥸�塼��

%description
This module is to guess file type from its contents like file(1)
command.

%description -l ja
���Υ⥸�塼��ϡ�file(1)���ޥ�ɤ��������뵡ǽ�����ʤ���ե��������
�Ƥ��餽�μ�����¬���뤿��Τ�ΤǤ���


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
