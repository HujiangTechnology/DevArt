package com.hujiang.devart.security

import android.util.Log
import org.bouncycastle.asn1.ASN1InputStream
import org.bouncycastle.asn1.ASN1ObjectIdentifier
import org.bouncycastle.asn1.DEROutputStream
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers
import org.bouncycastle.cert.jcajce.JcaCertStore
import org.bouncycastle.cms.CMSProcessableByteArray
import org.bouncycastle.cms.CMSSignedDataGenerator
import org.bouncycastle.cms.CMSTypedData
import org.bouncycastle.cms.jcajce.JcaSignerInfoGeneratorBuilder
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder
import org.bouncycastle.operator.jcajce.JcaDigestCalculatorProviderBuilder
import org.bouncycastle.util.encoders.Base64
import java.io.*
import java.nio.charset.Charset
import java.security.*
import java.security.cert.CertificateFactory
import java.security.cert.X509Certificate
import java.security.spec.InvalidKeySpecException
import java.security.spec.KeySpec
import java.security.spec.PKCS8EncodedKeySpec
import java.util.*
import java.util.jar.*
import java.util.regex.Pattern

/**
 * Created by rarnu on 4/8/16.
 */
class SignApk {

    companion object {
        private val CERT_SF_NAME = "META-INF/CERT.SF"
        private val CERT_RSA_NAME = "META-INF/CERT.RSA"
        private val OTACERT_NAME = "META-INF/com/android/otacert"
        private var _bouncyCastleProvider: Provider? = null
        private var _stripPattern = Pattern.compile("^(META-INF/((.*)[.](SF|RSA|DSA)|com/android/otacert))|(" + Pattern.quote(JarFile.MANIFEST_NAME) + ")$")

        fun addDigestsToManifest(jar: JarFile?): Manifest? {
            val input = jar?.manifest
            val output = Manifest()
            val main = output.mainAttributes
            if (input != null) {
                main.putAll(input.mainAttributes)
            } else {
                main.putValue("Manifest-Version", "1.0")
                main.putValue("Created-By", "DevArt SignApk")
            }
            val md = MessageDigest.getInstance("SHA1")
            val buffer = ByteArray(4096)
            val byName = TreeMap<String, JarEntry>()
            val e = jar?.entries()
            while (e!!.hasMoreElements()) {
                val entry = e.nextElement()
                byName.put(entry.name, entry)
            }
            for (entry in byName.values) {
                val name = entry.name
                if (!entry.isDirectory && (_stripPattern == null || !_stripPattern.matcher(name).matches())) {
                    val data = jar!!.getInputStream(entry)
                    var num: Int
                    while (true) {
                        num = data.read(buffer)
                        if (num > 0) {
                            md.update(buffer, 0, num)
                        } else {
                            break
                        }
                    }
                    var attr: Attributes? = null
                    if (input != null) {
                        attr = input.getAttributes(name)
                    }
                    attr = if (attr != null) Attributes(attr) else Attributes()
                    attr.putValue("SHA1-Digest", String(Base64.encode(md.digest()), Charset.forName("ISO-8859-1")))
                    output.entries.put(name, attr)
                }
            }
            return output
        }

        private fun copyFiles(manifest: Manifest?, inJar: JarFile?, out: JarOutputStream?, timestamp: Long) {
            val buffer = ByteArray(4096)
            val entries = manifest?.entries
            val names = ArrayList<String>(entries?.keys)
            Collections.sort(names)
            var num: Int
            for (name in names) {
                val inEntry = inJar?.getJarEntry(name)
                var outEntry: JarEntry
                if (inEntry?.method == JarEntry.STORED) {
                    outEntry = JarEntry(inEntry)
                } else {
                    outEntry = JarEntry(name)
                }
                outEntry.time = timestamp
                out?.putNextEntry(outEntry)
                val data = inJar?.getInputStream(inEntry)
                while (true) {
                    num = data!!.read(buffer)
                    if (num > 0) {
                        out?.write(buffer, 0, num)
                    } else {
                        break
                    }
                }
                out?.flush()
            }
        }

        fun signFile(manifest: Manifest?, inputJar: JarFile?, publicKey: X509Certificate?, privateKey: PrivateKey?, outputJar: JarOutputStream?) {
            val timestamp = publicKey!!.notBefore.time + 3600L * 1000
            copyFiles(manifest, inputJar, outputJar, timestamp)
            var je = JarEntry(JarFile.MANIFEST_NAME)
            je.time = timestamp
            outputJar?.putNextEntry(je)
            manifest?.write(outputJar)
            je = JarEntry(CERT_SF_NAME)
            je.time = timestamp
            outputJar?.putNextEntry(je)
            val baos = ByteArrayOutputStream()
            writeSignatureFile(manifest, baos)
            val signedData = baos.toByteArray()
            outputJar?.write(signedData)
            je = JarEntry(CERT_RSA_NAME)
            je.time = timestamp
            outputJar?.putNextEntry(je)
            writeSignatureBlock(CMSProcessableByteArray(signedData), publicKey, privateKey, outputJar)
        }

        private fun writeSignatureFile(manifest: Manifest?, out: OutputStream?) {
            val sf = Manifest()
            val main = sf.mainAttributes
            main.putValue("Signature-Version", "1.0")
            main.putValue("Created-By", "DevArt SignApk")
            val md = MessageDigest.getInstance("SHA1")
            val print = PrintStream(DigestOutputStream(ByteArrayOutputStream(), md), true, "UTF-8")
            manifest?.write(print)
            print.flush()
            main.putValue("SHA1-Digest-Manifest", String(Base64.encode(md.digest()), Charset.forName("ISO-8859-1")))
            val entries = manifest?.entries
            for ( entry in entries!!.entries) {
                print.print("Name: ${entry.key}\r\n")
                for (att in entry.value.entries) {
                    print.print("${att.key}: ${att.value}\r\n")
                }
                print.print("\r\n")
                print.flush()
                val sfAttr = Attributes()
                sfAttr.putValue("SHA1-Digest", String(Base64.encode(md.digest()), Charset.forName("ISO-8859-1")))
                sf.entries.put(entry.key, sfAttr)
            }
            val cout = CountOutputStream(out)
            sf.write(cout)
            if (cout.size() % 1024 == 0) {
                cout.write("\r".toInt())
                cout.write("\n".toInt())
            }
        }

        fun addOtacert(outputJar: JarOutputStream?, publicKeyFile: File?, timestamp: Long, manifest: Manifest?) {
            val md = MessageDigest.getInstance("SHA1")
            val je = JarEntry(OTACERT_NAME)
            je.time = timestamp
            outputJar?.putNextEntry(je)
            val input = FileInputStream(publicKeyFile)
            val b = ByteArray(4096)
            var read: Int
            while(true) {
                read = input.read(b)
                if (read != -1) {
                    outputJar?.write(b, 0, read)
                    md.update(b, 0, read)
                } else {
                    break
                }
            }
            input.close()
            val attr = Attributes()
            attr.putValue("SHA1-Digest", String(Base64.encode(md.digest()), Charset.forName("ISO-8859-1")))
            manifest?.entries?.put(OTACERT_NAME, attr)
        }

        fun writeSignatureBlock(data: CMSTypedData, publicKey: X509Certificate?, privateKey: PrivateKey?, out: OutputStream?) {
            val certList = ArrayList<X509Certificate?>(1)
            certList.add(publicKey)
            val certs = JcaCertStore(certList)
            val gen = CMSSignedDataGenerator()
            val sha1Signer = JcaContentSignerBuilder("SHA1withRSA").setProvider(_bouncyCastleProvider).build(privateKey)
            gen.addSignerInfoGenerator(JcaSignerInfoGeneratorBuilder(JcaDigestCalculatorProviderBuilder().setProvider(_bouncyCastleProvider).build()).setDirectSignature(true).build(sha1Signer, publicKey))
            gen.addCertificates(certs)
            val sigData = gen.generate(data, false)
            val asn1 = ASN1InputStream(sigData.encoded)
            val dos = DEROutputStream(out)
            dos.writeObject(asn1.readObject())
        }

        private fun readPublicKey(file: File?): X509Certificate? {
            val input = FileInputStream(file)
            try {
                val cf = CertificateFactory.getInstance("X.509")
                return cf.generateCertificate(input) as X509Certificate?
            } finally {
                input.close()
            }
        }

        private fun readPrivateKey(file: File?): PrivateKey? {
            val input = DataInputStream(FileInputStream(file))
            try {
                val bytes = ByteArray(file!!.length().toInt())
                input.read(bytes)
                val spec = PKCS8EncodedKeySpec(bytes)
                try {
                    return KeyFactory.getInstance("RSA").generatePrivate(spec)
                } catch (e: InvalidKeySpecException) {
                    return KeyFactory.getInstance("DSA").generatePrivate(spec)
                }
            } finally {
                input.close()
            }
        }

        private fun signWholeFile(inputJar: JarFile?, publicKeyFile: File?, publicKey: X509Certificate?, privateKey: PrivateKey?, outputStream: OutputStream?) {
            val cmsOut = Signer(inputJar, publicKeyFile, publicKey, privateKey, outputStream)
            val temp = ByteArrayOutputStream()
            val message = "signed by SignApk".toByteArray(Charset.forName("UTF-8"))
            temp.write(message)
            temp.write(0)
            cmsOut.writeSignatureBlock(temp)
            val zipData = cmsOut.getSigner()!!.getTail()!!
            if (zipData[zipData.size - 22] != 0x50.toByte() || zipData[zipData.size - 21] != 0x4b.toByte() || zipData[zipData.size - 20] != 0x05.toByte() || zipData[zipData.size - 19] != 0x06.toByte()) {
                throw IllegalArgumentException("zip data already has an archive comment")
            }
            val totalSize = temp.size() + 6
            if (totalSize > 0xffff) {
                throw IllegalArgumentException("signature is too big for ZIP file comment")
            }
            val signatureStart = totalSize - message.size - 1
            temp.write(signatureStart and 0xff)
            temp.write((signatureStart shr 8) and 0xff)
            temp.write(0xff)
            temp.write(0xff)
            temp.write(totalSize and 0xff)
            temp.write((totalSize shr 8) and 0xff)
            temp.flush()
            val b = temp.toByteArray()
            for (i in 0 .. b.size - 3 -1) {
                if (b[i] == 0x50.toByte() && b[i + 1] == 0x4b.toByte() && b[i + 2] == 0x05.toByte() && b[i + 3] == 0x06.toByte()) {
                    throw IllegalArgumentException("found spurious EOCD header at " + i)
                }
            }
            outputStream?.write(totalSize and 0xff)
            outputStream?.write((totalSize shr 8) and 0xff)
            temp.writeTo(outputStream)
        }

        fun sign(publicKeyFile: String?, privateKeyFile: String?, inputApk: String?, outputApk: String?, signWholeFile: Boolean) {
            _bouncyCastleProvider = BouncyCastleProvider()
            Security.addProvider(_bouncyCastleProvider)
            val inputFilename = inputApk
            val outputFilename = outputApk
            var inputJar: JarFile? = null
            var outputFile: FileOutputStream? = null
            try {
                val firstPublicKeyFile = File(publicKeyFile)
                val publicKey = readPublicKey(File(publicKeyFile))
                val privateKey = readPrivateKey(File(privateKeyFile))
                inputJar = JarFile(File(inputFilename), false)
                outputFile = FileOutputStream(outputFilename)
                if (signWholeFile) {
                    SignApk.signWholeFile(inputJar, firstPublicKeyFile, publicKey, privateKey, outputFile)
                } else {
                    val outputJar = JarOutputStream(outputFile)
                    outputJar.setLevel(9)
                    signFile(addDigestsToManifest(inputJar), inputJar, publicKey, privateKey, outputJar)
                    outputJar.close()
                }
            } catch (e: Exception) {
                Log.e("LOG", "sign => ${e.message}")
            } finally {
                inputJar?.close()
                outputFile?.close()
            }
        }

    }

    class Signer : CMSTypedData {
        private var _type: ASN1ObjectIdentifier? = null
        private var _inputJar: JarFile? = null
        private var _publicKeyFile: File? = null
        private var _publicKey: X509Certificate? = null
        private var _privateKey: PrivateKey? = null
        private var _outputStream: OutputStream? = null
        private var _signer: WholeFileSignerOutputStream? = null

        constructor(inputJar: JarFile?, publicKeyFile: File?, publicKey: X509Certificate?, privateKey: PrivateKey?, outputStream: OutputStream?) {
            _inputJar = inputJar
            _publicKeyFile = publicKeyFile
            _publicKey = publicKey
            _privateKey = privateKey
            _outputStream = outputStream
            _type = ASN1ObjectIdentifier(CMSObjectIdentifiers.data.id)
        }

        override fun getContent(): Any? {
            throw UnsupportedOperationException()
        }

        override fun getContentType(): ASN1ObjectIdentifier? = _type

        override fun write(out: OutputStream?) {

            _signer = WholeFileSignerOutputStream(out, _outputStream)
            val outputJar = JarOutputStream(_signer)
            val manifest = SignApk.addDigestsToManifest(_inputJar)
            SignApk.signFile(manifest, _inputJar, _publicKey, _privateKey, outputJar)
            val timestamp = _publicKey!!.notBefore.time + 3600L * 1000
            SignApk.addOtacert(outputJar, _publicKeyFile, timestamp, manifest)
            _signer?.notifyClosing()
            outputJar.close()
            _signer?.finish()
        }

        fun writeSignatureBlock(temp: ByteArrayOutputStream?) {
            SignApk.writeSignatureBlock(this, _publicKey, _privateKey, temp)
        }

        fun getSigner(): WholeFileSignerOutputStream? = _signer
    }

    class CountOutputStream : FilterOutputStream {
        private var _count = 0

        constructor(out: OutputStream?) : super(out) {
            _count = 0
        }

        override fun write(b: Int) {
            super.write(b)
            _count++
        }

        override fun write(buffer: ByteArray?, offset: Int, length: Int) {
            super.write(buffer, offset, length)
            _count += length
        }

        fun size(): Int = _count
    }


    class WholeFileSignerOutputStream : FilterOutputStream {
        private var _closing = false
        private var _footer = ByteArrayOutputStream()
        private var _tee: OutputStream? = null

        constructor(out: OutputStream?, tee: OutputStream?) : super(out) {
            _tee = tee
        }

        fun notifyClosing() {
            _closing = true
        }

        fun finish() {
            _closing = false
            val data = _footer.toByteArray()
            if (data.size < 2) {
                throw IOException("Less than two bytes written to footer")
            }
            write(data, 0, data.size - 2)
        }

        fun getTail(): ByteArray? = _footer.toByteArray()

        override fun write(buffer: ByteArray?) {
            write(buffer, 0, buffer!!.size)
        }

        override fun write(buffer: ByteArray?, offset: Int, length: Int) {
            if (_closing) {
                _footer.write(buffer, offset, length)
            } else {
                out.write(buffer, offset, length)
                _tee?.write(buffer, offset, length)
            }
        }

        override fun write(b: Int) {
            if (_closing) {
                _footer.write(b)
            } else {
                out.write(b)
                _tee?.write(b)
            }
        }
    }
}