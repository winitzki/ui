package io.chymyst.ui.util


import java.net.NetworkInterface
import java.security.SecureRandom
import java.time.Instant


/** Ported from https://www.callicoder.com/distributed-unique-id-sequence-number-generator/
 * Distributed Sequence Generator.
 * Inspired by Twitter snowflake: https://github.com/twitter/snowflake/tree/snowflake-2010
 *
 * This class should be used as a Singleton.
 * Make sure that you create and reuse a Single instance of SequenceGenerator per node in your distributed system cluster.
 */
object SequenceGenerator {
  private val UNUSED_BITS = 1 // Sign bit, Unused (always set to 0)

  private val EPOCH_BITS = 41
  private val NODE_ID_BITS = 10
  private val SEQUENCE_BITS = 12
  private val maxNodeId = (Math.pow(2, NODE_ID_BITS) - 1).toInt
  private val maxSequence = (Math.pow(2, SEQUENCE_BITS) - 1).toInt
  // Custom Epoch (January 1, 2015 Midnight UTC = 2015-01-01T00:00:00Z)
  private val CUSTOM_EPOCH = 1420070400000L

  // Get current timestamp in milliseconds, adjust for the custom epoch.
  private def timestamp = Instant.now.toEpochMilli - CUSTOM_EPOCH
}

class SequenceGenerator // Let SequenceGenerator generate a nodeId
{
  final private var nodeId = createNodeId
  @volatile private var lastTimestamp = -1L
  @volatile private var sequence = 0L

  def nextId: Long = {
    var currentTimestamp = SequenceGenerator.timestamp
    if (currentTimestamp < lastTimestamp) throw new IllegalStateException("Invalid System Clock!")
    if (currentTimestamp == lastTimestamp) {
      sequence = (sequence + 1) & SequenceGenerator.maxSequence
      if (sequence == 0) {
        // Sequence Exhausted, wait till next millisecond.
        currentTimestamp = waitNextMillis(currentTimestamp)
      }
    }
    else {
      // reset sequence to start with zero for the next millisecond
      sequence = 0
    }
    lastTimestamp = currentTimestamp
    var id = currentTimestamp << (SequenceGenerator.NODE_ID_BITS + SequenceGenerator.SEQUENCE_BITS)
    id |= (nodeId << SequenceGenerator.SEQUENCE_BITS)
    id |= sequence
    id
  }

  // Block and wait till next millisecond
  private def waitNextMillis(currentTimestamp: Long) = {
    var newTimestamp: Long = currentTimestamp
    while (newTimestamp == lastTimestamp) newTimestamp = SequenceGenerator.timestamp
    newTimestamp
  }

  private def createNodeId = {
    var nodeId = 0
    try {
      val sb = new StringBuilder
      val networkInterfaces = NetworkInterface.getNetworkInterfaces
      while (networkInterfaces.hasMoreElements) {
        val networkInterface = networkInterfaces.nextElement
        val mac = networkInterface.getHardwareAddress
        if (mac != null) for (i <- 0 until mac.length) {
          sb.append(String.format("%02X", mac(i)))
        }
      }
      nodeId = sb.toString.hashCode
    } catch {
      case ex: Exception =>
        nodeId = new SecureRandom().nextInt
    }
    nodeId = nodeId & SequenceGenerator.maxNodeId
    nodeId
  }
}
