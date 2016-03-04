object FB_uploader_3 extends App {

	import java.io.BufferedReader;
	import java.io.IOException;
	import java.io.InputStreamReader;
	import java.io.FileInputStream;
	import java.lang.NullPointerException
	import java.net.SocketTimeoutException
	import java.io.FileWriter
	import java.io.BufferedWriter
	import com.restfb.DefaultFacebookClient
	import com.restfb.Parameter
	import com.restfb.BinaryAttachment
	import com.restfb.batch.BatchRequest
	import com.restfb.types._
	import com.restfb.exception.FacebookOAuthException
	
	val br = new BufferedReader(new InputStreamReader(System.in))
	val br2 = new BufferedReader(new InputStreamReader(System.in))
	val br3 = new BufferedReader(new InputStreamReader(System.in))
	
	def enterPath(firstTry:Boolean):String = {
		if(firstTry == true) {
			println("Copy your files to their own directory,")
			println("then enter the full path name: ")	
		}
		else println("enter the correct path:")
		val path = br.readLine()
		println(s"upload files from $path?  (y/n)")
		val reply = br2.readLine()
		test(reply, path)
		}

	def test(confirm:String, testPath:String): String = confirm match {
		case y if List("y", "yes").contains(confirm.toLowerCase) => testPath
		case _ => {
			enterPath(false)
	}
		}
		
		def uploadWithStatus(i:Tuple4[String, java.io.File, Long, String], fbClient: DefaultFacebookClient):String= {
			val path = i._4
			val name = i._1
			val length = i._3
			val stream = new java.io.FileInputStream(i._2)
			println(s"uploading file $name length: $length bytes")
			fbClient.publish(s"me/$path", classOf[FacebookType], BinaryAttachment.`with`(i._1,  stream))
			stream.close
			println(s"uploaded file $name")
			path
		}
		
		val all = 
			try {
				val d = new java.io.File(enterPath(true))
		    val all = d.listFiles.filter(_.isFile).toList
		    all
		    }
		catch {
			case ex: java.lang.NullPointerException => println("folder does not exist"); Nil
			}
		
	    	if (all != Nil) {
		    	val photos = for(item <- all if (item.toString endsWith "jpg")) yield (item.getName, item, item.length, "photos")
		    	val videos = for(item <- all if !(item.toString endsWith "jpg")) yield (item.getName, item, item.length, "videos")
	    		println("paste in the authorization token:");
	    		val auth_token = br3.readLine()
    			try{
						val fbClient = new DefaultFacebookClient(auth_token)
						if(photos != Nil) {
							println("photos found...")
							val publishPhotoResponse = photos.map(i=> uploadWithStatus(i, fbClient))}
						else println("no photos found")
						if(videos != Nil) {
							println("videos found...")
							val publishVideoResponse = videos.map(i=> uploadWithStatus(i, fbClient))}
						else println("no videos found")
    			}
					catch {
						case ex: com.restfb.exception.FacebookOAuthException => println(ex)
						case ex: java.lang.NullPointerException => println(s"found nothing"); test("n", br2.readLine())
						case ex: java.net.SocketTimeoutException => println("there was a socket timeout error")
    			}
	    	}
	    	else println("no files found")
}
