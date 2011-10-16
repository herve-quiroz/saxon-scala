/*
 * Copyright 2011 Herve Quiroz
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License.  You may obtain a copy
 * of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
 * License for the specific language governing permissions and limitations
 * under the License.
 */
package org.trancecode.s9api

import scala.collection.immutable.List
import scala.collection.JavaConversions
import net.sf.saxon.s9api.Axis
import net.sf.saxon.s9api.XdmItem
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.XdmNodeKind
import net.sf.saxon.s9api.QName
import scala.collection.immutable.Map
import scala.collection.immutable.HashMap
import net.sf.saxon.s9api.Processor
import javax.xml.transform.stream.StreamSource
import java.io.StringReader;
import com.google.common.io.Closeables
import net.sf.saxon.s9api.SaxonApiException

object Saxon {

  def getAttributeAsQName(element: XdmNode, attributeName: QName): QName = {
    val value = element.getAttributeValue(attributeName)
    new QName(value, element)
  }

  def getAttributeMap(element: XdmNode): Map[QName, String] = {
    getAttributes(element).map({ node => asAttributeMapEntry(node) }).toMap
  }

  private def asAttributeMapEntry(attribute: XdmNode): (QName, String) = {
    (attribute.getNodeName(), attribute.getStringValue())
  }

  def getAttributes(element: XdmNode): Iterable[XdmNode] = {
    getFromAxis(element, Axis.ATTRIBUTE)
  }

  def getChildElements(element: XdmNode): Iterable[XdmNode] = {
    getFromAxis(element, Axis.CHILD).filter(isElement(_))
  }

  def getChildElements(element: XdmNode, names: QName*): Iterable[XdmNode] = {
    getChildElements(element).filter(node => names contains node.getNodeName())
  }

  private def getFromAxis(node: XdmNode, axis: Axis): Iterable[XdmNode] = {
    List.fromIterator(asXdmNodes(JavaConversions.asIterator(node.axisIterator(axis))))
  }

  def asXdmNodes(items: Iterator[XdmItem]) = {
    items.filter(isXdmNode(_)).map(_.asInstanceOf[XdmNode])
  }

  def asXdmNodes(items: Iterable[XdmItem]) = {
    items.filter(isXdmNode(_)).map(_.asInstanceOf[XdmNode])
  }

  def isAttribute(node: XdmNode) = node.getNodeKind() == XdmNodeKind.ATTRIBUTE

  def isElement(node: XdmNode) = node.getNodeKind() == XdmNodeKind.ELEMENT

  def isXdmNode(node: XdmItem) = node.isInstanceOf[XdmNode]

  def parse(content: String, processor: Processor): XdmNode = {
    val reader = new StringReader(content)
    try {
      processor.newDocumentBuilder().build(new StreamSource(reader));
    } catch {
      case e: SaxonApiException =>
        throw new IllegalStateException(e);
    } finally {
      Closeables.closeQuietly(reader);
    }
  }

}
