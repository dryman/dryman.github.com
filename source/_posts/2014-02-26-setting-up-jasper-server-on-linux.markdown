---
layout: post
title: "Setting up Jasper Server on Linux"
date: 2014-02-26 13:26
comments: true
categories: Jasper Java
---

Jasper is one of the standard report generator in the industry. However, setting up Jasper is a pain of ass. This post is my note for setting up Jasper on Linux, in case I have to do it again in the future...

<!--more-->

### Setting up an Ubuntu instance on Amazon

The first thing you need to do when you launch an Amazon EC2 instance is add the hostname to hosts. Copy `/etc/hostname` to `/etc/hosts` like so:

```
127.0.0.1 localhost ip-10-0-0-1
```

The `ip-10-0-0-1` is the host name in your `/etc/hostname`. You'll need this setup else the jasper installation script might fail.

### Update aptitude and install PostgresQL

This section is referenced from [Ubuntu PostgreSQL HowTo][postgres]

```bash
$ sudo apt-get update
$ sudo apt-get install postgresql
```

After Postgres is installed, you'll need to setup its permission and password, so that jasper can connect to it.

First, enable postgres to be reached from localhost. Edit `/etc/postgresql/9.1/main/postgresql.conf` and un-comment this line:

```
listen_addresses = 'localhost'
```

Then, login to postgres using postgres user:

```bash
$ sudo -u postgres psql
```

You should be able to login without using password. Now, update your password:

```sql
postgres=# ALTER USER postgres with encrypted password 'your_password';
postgres=# \q
```

After setting the password, edit the file `/etc/postgresql/9.1/main/pg_hba.conf` and set the `postgres` user to use md5 authentication.

```
local   all             postgres                                md5
```

Finally, restart the postgres server:

```bash
$ sudo /etc/init.d/postgres restart
```

[postgres]: https://help.ubuntu.com/10.04/serverguide/postgresql.html


### Download and run jasperserver install script

Got to [Jaspersoft Downloads][jasperdownload] and get the `jasperreports-server-cp-5.5.0-linux-x64-installer.run` script. In our case we run it as super user.

```bash
$ chmod a+x jasperreports-server-cp-5.5.0-linux-x64-installer.run
$ sudo ./jasperreports-server-cp-5.5.0-linux-x64-installer.run
```

You'll go through several steps:

1. It prints out the license. Read through it and select `y` when the prompt `Do you accept this license? [y/n]: y` shows up.

2. It asks you to choose complete install or custom install. Choose **custom**.

3. It asks you to select a folder for install destination. I chose the default `/opt/jasperreports-server-cp-5.5.0`.

4. It asks you to use bundled tomcat or existing one. Choose **bundled**.

5. It asks you to use bundled postgres or existing one. Choose **existing**.

6. It asks you the Tomcat server port. Just use the default `8080`.

7. It asks you postgres binary location. Use `/usr/lib/postgresql/9.1/bin`.

7. It asks you host and port for postgres server. Use the default `127.0.0.1` and `5432`.

8. It asks you the postgres password. Type it in.

9. It asks you whether to install sample database and sample reports. Pick whatever you like.

10. Proceed the rest of the installation.


Once you're done with the installation. You can start the tomcat server using the command

```bash
$ /opt/jasperreports-server-cp-5.5.0/ctlscript.sh start
```

[jasperdownload]: http://community.jaspersoft.com/download


### Setup plsql query adapter

This section is referenced to [Jaspersoft wiki][jasperwiki]. We found that when selecting query language as `plsql`, jasperserver rejects the uploaded report.

To solve the issue, you'll need to extend jasperserver with this plugin: [jasperreports-extensions-3.5.3.jar](http://www.java2s.com/Code/Jar/j/Downloadjasperreportsextensions353jar.htm). Download the jar, and place it to `/opt/jasperreports-server-cp-5.5.0/apache-tomcat/webapps/jasperserver/WEB-INF/lib/`.

Now, cd to `/opt/jasperreports-server-cp-5.5.0/apache-tomcat/webapps/jasperserver/WEB-INF`, and add one line to `classes/jasperreports.properties`.

```
net.sf.jasperreports.query.executer.factory.plsql=com.jaspersoft.jrx.query.PlSqlQueryExecuterFactory
```

Edit `flows/queryBeans.xml`. Change the line `<property name="queryLanguages" ref="queryLanguages">` to

```xml
<property name="queryLanguages" ref="queryLanguages">
  <list>
    <value>sql</value>
    <value>plsql</value>
    <value>hql</value>
  </list>
</property>
```

Edit `applicationContext.xml`. Search for `supportedQueryLanguages` and change it to the following.

```xml
<bean class="com.jaspersoft.jasperserver.api.engine.jasperreports.util.DataSourceServiceDefinition">
    <property name="serviceBeanName" value="jdbcDataSourceServiceFactory"/>
    <property name="supportedQueryLanguages">
        <set>
            <value>sql</value>
            <value>SQL</value>
 
            <!-- Add these two value lines for the Oracle PL/SQL Language -->
            <value>plsql</value>
            <value>PLSQL</value>
 
        </set>
    </property>
</bean>
 
<bean class="com.jaspersoft.jasperserver.api.engine.jasperreports.util.DataSourceServiceDefinition">
    <property name="serviceBeanName" value="jndiJdbcDataSourceServiceFactory"/>
    <property name="supportedQueryLanguages">
        <set>
            <value>sql</value>
            <value>SQL</value>
 
            <!-- Add these two value lines for the Oracle PL/SQL Language -->
            <value>plsql</value>
            <value>PLSQL</value>
 
            </set>
        </property>
</bean>
```

Finally, restart the jasper server.


```bash
$ /opt/jasperreports-server-cp-5.5.0/ctlscript.sh stop
$ /opt/jasperreports-server-cp-5.5.0/ctlscript.sh start
```

[jasperwiki]: http://community.jaspersoft.com/wiki/no-query-executer-factory-registered-plsql-language-jasperserver-51

### Increase Jasper memory usage

More specifically, tomcat's memory usage. The default memory setting is quite low and jasper is pretty in-responsive to user clicks. To change the setting, edit `/opt/jasperreports-server-cp-5.5.0/apache-tomcat/scripts/ctl.sh` and change the two `JAVA_OPTS` to

```
export JAVA_OPTS="-server -Xms2048m -Xmx2048m -XX:PermSize=256m -XX:MaxPermSize=512m -Xss2m -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Djava.awt.headless=true"
```

Relaunch your jasper server again. Now it should run more smoothly.

## Conclusion

ITS A PAIN TO RUN JASPER...

Thanks to all the people that wrote wiki and articles to help me out. I hope I'll never need to set it up again.
