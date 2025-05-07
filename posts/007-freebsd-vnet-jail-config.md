---
title: FreeBSD VNET Jail 구성 방법
published: 2024-09-25T15:27:41+09:00
updated: 2025-05-07T18:30:00+09:00
---

FreeBSD의 VNET Jail이 무엇인지, 어디에 쓰는지 소개한다. 구성 과정에서 마주칠 수
있는 문제와 해결 방안을 다룬다.

## FreeBSD Jail

FreeBSD의 **Jail** 기능은 프로세스, 파일 시스템, 네트워크, 사용자 및 권한을
격리하는 환경을 제공한다. Linux의 **Docker**와 비교하자면, Jail은 Docker와 달리
운영체제의 커널 레벨에서 지원하는 기능이다. 따라서 운영체제와 밀접히 통합되어
있으며, 보안 및 안정성, 자원 관리, 네트워크 분리, ZFS 파일 시스템의 활용 등
강력한 장점이 있다.

사실 FreeBSD Jail과 직접적으로 비교할 수 있는 대상은 Docker보다는 LXC가 더
적합해보인다. LXC는 리눅스 커널 차원에서 지원하는 기능이고, 한때 Docker도 LXC를
기반으로 개발되었던 것으로 알고 있다. 지금은 runc라는 별도의 컨테이너 런타임을
사용하고 있다.

Docker는 개발 편의를 위한 도구로써 유용하다. Docker 이미지의 생성 및
레포지토리를 통한 배포 등 개발 환경의 구축을 간소화하고 편의를 제공해주는
도구라는 측면에서 장점이 분명하다. 하지만 모든 컨테이너가 하나의 docker
daemon에 의해 관리되고, 이 프로세스가 root 권한으로 실행된다는 점에서 근본적인
보안 문제가 있다.

이를 해결한 Docker의 대안으로는 **Podman**이 있는데, 각 컨테이너들이 독립적으로
실행되고 이들을 관리하기 위한 daemon 또한 존재하지 않으며, 루트 권한 없이
실행[^rootless-podman-freebsd]되므로 보안상 안전하다. FreeBSD에서도 ocijail
런타임을 활용하여 Podman을 사용할 수 있다. ocijail은 Jail을 활용하여 구현한
컨테이너 런타임이다. 따라서 FreeBSD에서 Docker와 같은 개발 편의 도구가
필요하다면 Podman을 활용할 수 있다.

[^rootless-podman-freebsd]: 리눅스에서 지원. 아쉽게도 아직까지 FreeBSD에서의
    Podman은 루트 권한 없이 컨테이너를 생성하지 못한다.

본론으로 돌아와서, FreeBSD에서는 Podman 등을 사용하지 않아도 기본적으로
제공하는 Jail 기능만 활용하더라도 강력한 격리 환경을 구성할 수 있다.

## VNET Jail

> A FreeBSD VNET jail is a virtualized environment that allows for the
> isolation and control of network resources for processes running within it.
> It provides a high level of network segmentation and security by creating a
> separate network stack for processes within the jail, ensuring that network
> traffic within the jail is isolated from the host system and other jails.
> \
> – [FreeBSD Handbook][handbook-vnet], **17.2.4. VNET Jails.**

[handbook-vnet]: https://docs.freebsd.org/en/books/handbook/jails/#vnet-jails


**VNET Jail**은 호스트와 완전히 분리된 네트워크 스택(L2~L7 계층)을 가진다.
즉, 별도의 가상 이더넷 인터페이스(epair, vnet)로 고유한 MAC·L2 링크를 할당받고,
브리지나 VLAN 등 L2 수준에서 자유롭게 구성할 수 있다는 뜻이다. 이로 인해 VNET
Jail은 다음과 같은 특징을 갖는다.

1. **VM과 마찬가지로 호스트와 별개의 IP를 할당받을 수 있다.** 독립된 L2
   인터페이스를 통해 호스트와 다른 서브넷에 IP를 붙일 수 있으며, IP aliasing
   없이도 완전히 분리된 라우팅·방화벽 정책을 적용 가능하다.

2. **고유의 MAC 주소를 가진 가상 NIC를 사용한다.** epair 인터페이스를 통해 L2
   프레임을 주고받으며, 브리지(bridge)나 스위치에 직접 연결할 수 있다.

3. **브리지·VLAN·터널링 같은 L2 기능을 그대로 활용할 수 있다.** 호스트의
   bridge0에 VNET Jail의 인터페이스를 addm 하거나, VLAN 태그를 트렁킹해서
   동일한 L2 도메인에 참여시킬 수 있다.

4. **방화벽·라우팅·네트워크 네임스페이스가 호스트와 완전히 분리된다.** Jail
   내부 전용 방화벽 규칙을 운영하고, routing table을 별도로 관리할 수 있다.
   Non-VNET Jail에서는 policy-based routing으로 라우팅을 분리할 수 있다.

 반면 Non-VNET Jail은 호스트의 물리 NIC가 속한 동일 L2 도메인(같은 MAC 링크)을
 공유하고, IP aliasing을 통해 같은 서브넷 내에서만 IP를 할당받는다. 따라서 L2
 계층부터 완전 분리가 필요한 경우에는 반드시 VNET Jail을 사용해야 한다.

### VNET Jail의 활용

운용 예시를 살펴보면 VNET Jail의 매력을 이해하는 데 도움이 된다. VNET Jail은
호스트와 다른 네트워크 인터페이스를 가지므로 각 Jail을 서로 다른 VLAN에 둘 수
있다. 또한 라우팅 테이블이 호스트와 다르므로 일부 Jail을 특정한 VPN에
연결하도록 구성할 수 있다.

개인적으로 VNET Jail을 활용하여 호스트와 각 Jail에 고유한 IP를 부여하여 다양한
웹 서비스를 운영하고 있다. 또한, **dnsmasq**로 로컬 DNS 서버를 구축해두고 각
서비스에 서로 다른 로컬 도메인 이름을 매핑해 두었다. 하나의 reverse proxy
아래에 모든 서비스를 두는 방법도 있었지만, 이렇게 네트워크를 구성한 이유는 각
Jail을 물리적으로 구분되는 별도의 서버로 가정해 운영해보고 싶었기 때문이다.

또한 VLAN을 나누어, 각 Jail을 서로 다른 VLAN에 배치하는 구성도 실험하고자 했다.
VNET Jail은 호스트 및 다른 VNET Jail들과 각각 독립된 네트워크 인터페이스를
가지기 때문에, 각 Jail을 서로 다른 VLAN에 두면 이들 간의 통신은 반드시 라우터를
거쳐야만 이루어질 수 있다. 이러한 구성을 통해 각 VLAN에 각각 다른 방화벽 정책을
설정하는 등의 실험을 진행할 수 있었다.

홈 네트워크 구성 또한 나중에 기회가 되면 포스팅하겠다.

### VNET Jail 구성 방법

VNET Jail은 epair 인터페이스를 생성하여 한 쪽은 브릿지에, 다른 쪽은 Jail에
연결하도록 구성한다.

다음 내용을 `/etc/rc.conf`에 추가한다.

```unix
cloned_interface="bridge0"

ifconfig_bridge0="addm em0 up"
```

> **Note.** `em0`와 같은 인터페이스 이름은 기기마다 다를 수 있으니 그대로
> 사용할 수는 없다.

다음 내용을 `/etc/jail.conf`에 추가한다.

```unix
my-vnet-jail {
# ...

# VNET/VIMAGE
  vnet;
  vnet.interface = "${epair}b";

# NETWORKS/INTERFACES
  $id = "154";
  $ip = "192.168.1.${id}/24";
  $gateway = "192.168.1.1";
  $bridge = "bridge0";
  $epair = "epair${id}";

# ADD TO bridge INTERFACE
  exec.prestart  = "/sbin/ifconfig ${epair} create up";
  exec.prestart += "/sbin/ifconfig ${epair}a up descr jail:${name}";
  exec.prestart += "/sbin/ifconfig ${bridge} addm ${epair}a up";
  exec.start    += "/sbin/ifconfig ${epair}b ${ip} up";
  exec.start    += "/sbin/route add default ${gateway}";
  exec.poststop = "/sbin/ifconfig ${bridge} deletem ${epair}a";
  exec.poststop += "/sbin/ifconfig ${epair}a destroy";
}
```

자세한 내용은 [FreeBSD handbook][handbook-creating-vnet]를 참조한다.

[handbook-creating-vnet]: https://docs.freebsd.org/en/books/handbook/jails/#creating-vnet-jail

#### 문제점

간혹 Jail을 호스트에서 제거[^notation-remove]한 이후에도 `epair###b`
인터페이스가 Jail에서 release되지 않아서 호스트에서 보이지 않는
문제[^not-releasing-if]가 발생한다. 원래 `jail.conf`[^manpage-jail-conf]에서
`vnet.interface`의 인수로 설정한 인터페이스는 자동으로 release되어야
한다.[^manpage-jail]

[^notation-remove]:
    Jail의 **생성(create)/제거(remove)**라는 표현은 Jail을
    구성하는 **userland**의 생성/제거와 독립적이므로 주의해야 한다. Docker에
    익숙한 경우, Jail의 생성/제거는 Docker 컨테이너의
    생성(create)/제거(rm)보다는 **시작(start)/정지(stop)**와 더 비슷하다고
    이해할 수 있다.

[^not-releasing-if]: 해당 문제 보고는 [FreeBSD 포럼][forum-1] 참조.

[^manpage-jail-conf]: `man 5 jail.conf` 참조.

[^manpage-jail]:
    `man 8 jail` 참조:

    ```
    vnet.interface
            A network interface to give to a vnet-enabled jail after is it
            created.  The interface will automatically be released when the
            jail is removed.
    ```

[forum-1]: https://forums.FreeBSD.org/threads/interface-does-not-return-to-host-after-kill-jail.92730/post-648334

#### 해결 방법

Jail이 제거되는 시점에 호스트에서 `ifconfig -vnet` 명령어로 수동으로
인터페이스를 release해줄 수 있다. `jail.conf`의 `exec.prestop` 인수에 다음과
같이 명령어를 추가하면 된다.

```unix
my-vnet-jail {
# ...

# ADD TO bridge INTERFACE
  exec.prestart  = "/sbin/ifconfig ${epair} create up";
  exec.prestart += "/sbin/ifconfig ${epair}a up descr jail:${name}";
  exec.prestart += "/sbin/ifconfig ${bridge} addm ${epair}a up";
  exec.start    += "/sbin/ifconfig ${epair}b ${ip} up";
  exec.start    += "/sbin/route add default ${gateway}";

  # Add this line
  exec.prestop  += "/sbin/ifconfig ${epair}b || /sbin/ifconfig ${epair}b -vnet $name";

  exec.poststop = "/sbin/ifconfig ${bridge} deletem ${epair}a";
  exec.poststop += "/sbin/ifconfig ${epair}a destroy";
}
```

- `exec.stop`은 Jail 제거 시점에 Jail 내부에서 실행된다.
- `exec.prestop`과 `exec.poststop`은 각각 Jail 제거 직전과 직후에 호스트에서
  실행된다.

## Jail 구축 관련 참고 사항

### DHCP를 이용한 IP 할당

위에서 소개한 방법은 각 Jail의 IP를 수동으로 할당한다. DHCP를 이용하여 자동
할당받고자 하는 경우, 각 Jail에서 DHCP 클라이언트를 따로 구성해줘야 한다. 또한
`jail.conf`에서 각 Jail에 대해 일부 restriction을 추가로 해제할 필요가 있던
것으로 기억하는데, 정확한 방법은 기억나지 않는다.

### PostgreSQL 등 DB 구축

VNET Jail과 관련은 없으나, PostgreSQL과 같은 일부 데이터베이스[^other-dbs]를
Jail에서 정상적으로 구동하기 위해서는 해당 Jail에 별도의 restriction을 해제해야
한다. Jail을 소개할 때 언급했듯이, Jail은 각각 자원의 할당 및 권한 관리를
세세히 설정할 수 있다.

PostgreSQL의 경우, `sysvipc`[^manpage-jail-2] restriction을 해제해야 한다.
`jail.conf` 파일에 다음과 같이 추가한다.

```
my-postgres-jail {
# ...

# PERMISSIONS
  allow.sysvipc;
}
```

[^other-dbs]:
    SQLite는 따로 restriction의 해제를 요구하지 않았다.

[^manpage-jail-2]:
    `man 8 jail` 참조:

    ```
    allow.sysvipc
            A process within the jail has access to System V IPC
            primitives.  This is deprecated in favor of the per-
            module parameters (see below).  When this parameter is
            set, it is equivalent to setting sysvmsg, sysvsem, and
            sysvshm all to “inherit”.
    ```
