## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

### Issues
1. `failed with status 403 and body {"code":403,"msg":"Access denied"}`
   1. always return this error when access to export api even I add origin header.So give up for now...
2. no valid certification：
    ```bash
    javax.net.ssl.SSLHandshakeException: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target
    ```
    1. retrive `host` certificate to local `mubu.cert`:
        ```bash
        ./retrive_ssl.sh mubu.com
        ```
    2. import `mubu.cert` to java keystore, default keystore password is `changeit`:
        ```bash
        sudo keytool -import -file mubu.cert -alias mubu  -keystore $JAVA_HOME/lib/security/cacerts
        ``` 